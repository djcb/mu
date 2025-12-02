;;; mu4e-dbus.el --- DBus support -*- lexical-binding: t-*-

;; Copyright (C) 2025 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; mu4e is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; mu4e is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with mu4e.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Generic support for showing new-mail notifications.

;;; Code:

(eval-when-compile (require 'dbus nil 'noerror))
(require 'dbus nil 'noerror)

(require 'mu4e-helpers)
(require 'mu4e-query-items)
(require 'mu4e-context)

(defconst mu4e-dbus-service "nl.djcbsoftware.Mu4e"
  "Mu4e's DBus service name.")

(defconst mu4e-dbus-interface "nl.djcbsoftware.Mu4e"
  "Mu4e's DBus interface.")

(defconst mu4e-dbus-object-path "/nl/djcbsoftware/Mu4e"
  "Mu4e's DBus object path.")
;; We could use different object paths for allowing multiple mu4e's. But let's
;; not add that complication for now.

(defconst mu4e-dbus-introspection-xml
  "<node>
  <interface name='org.freedesktop.DBus.Introspectable'>
    <method name='Introspect'>
      <arg name='xml_data' type='s' direction='out'/>
    </method>
  </interface>
  <interface name='org.freedesktop.DBus.Properties'>
    <method name='Get'>
      <arg type='s' name='interface_name' direction='in'/>
      <arg type='s' name='property_name' direction='in'/>
      <arg type='v' name='value' direction='out'/>
    </method>
    <method name='GetAll'>
      <arg type='s' name='interface_name' direction='in'/>
      <arg type='a{sv}' name='properties' direction='out'/>
    </method>
    <method name='Set'>
      <arg type='s' name='interface_name' direction='in'/>
      <arg type='s' name='property_name' direction='in'/>
      <arg type='v' name='value' direction='in'/>
    </method>
    <signal name='PropertiesChanged'>
      <arg type='s' name='interface_name'/>
      <arg type='a{sv}' name='changed_properties'/>
      <arg type='as' name='invalidated_properties'/>
    </signal>
  </interface>
  <interface name='nl.djcbsoftware.Mu4e'>
    <property name='Version' type='s' access='read'/>
    <property name='DatabasePath' type='s' access='read'/>
    <property name='RootMaildir' type='s' access='read'/>
    <property name='Context' type='s' access='readwrite'/>
    <property name='QueryInfo' type='aa{sv}' access='readwrite'/>
  </interface>
</node>"
  "The XML blob used for DBus introspection.

Apart from the properties boilerplate, the
\"nl.djcbsoftware.Mu4e\" interface supports the following:

1) properties:
  - Version (string): the mu/mu4e version
  - Context (string): the current mu4e context, if any
  - QueryInfo (array-of-vardict):
    information about bookmarks / queries, i.e., a
    subset of the `mu4e-query-items' information.

    Each item describes a bookmark/maildir item with
    entries:
    - name     (string) ->  name of the item
    - query    (string) ->  query for this item
    - count    (number) ->  number of matching messages
    - unread   (number) ->  the number of unread messages
    - favorite (bool)   ->  this is the favorite entry
              (optional, at most one entry has this).")

;; Introspection... although not strictly required, it is very useful for
;; clients (including tools like d-spy) to find out the methods we support. We
;; need to implement introspection methods on each hierarchical level of the
;; object path... a lot of boilerplate
;;
;; I think Emacs could technically generate much of this, but it doesn't seem to
;; do so now....

(defun mu4e--dbus-introspect-parents ()
  "Set up introspection for the parent object paths."
  (seq-do
   (lambda (path-child)
     (let ((path (car path-child)) (child (cdr path-child)))
       (dbus-register-method
        :session  mu4e-dbus-service
        path
        dbus-interface-introspectable
        "Introspect"
        `(lambda ()
          ,(format "<node><node name='%s'/></node>" child)))))
   '(("/" . "nl")
    ("/nl" . "djcbsoftware")
    ("/nl/djcbsoftware" . "Mu4e"))))

(defun mu4e--dbus-introspect-nl-djcbsoftware-mu4e ()
  "Mu4e introspection for /nl/djcbsoftware/Mu4e."
  (dbus-register-method
   :session
   mu4e-dbus-service
   mu4e-dbus-object-path
   dbus-interface-introspectable
   "Introspect"
   (lambda () mu4e-dbus-introspection-xml)))

(defun mu4e--dbus-register-property (name access value)
  "Register mu4e DBus property with NAME to VALUE.

ACCESS is a symbol, either `:read', `:write' or `:readwrite'.

There are read-only, i.e. only writable from here. Thus,
setting a property means (re)registering it."
  (dbus-register-property
   :session
   mu4e-dbus-service
   mu4e-dbus-object-path
   mu4e-dbus-interface
   name access value))

(defun mu4e--dbus-set-property (name value)
  "Update mu4e DBus property with NAME to VALUE."
  (dbus-set-property
   :session
   mu4e-dbus-service
   mu4e-dbus-object-path
   mu4e-dbus-interface
   name value))

;; Convert to D-Bus format - list of dict entries
(defun mu4e--dbus-query-info ()
  "Return an array of dbus vardicts with query-items information.

This filters / maps the information from `mu4e-query-items'."
  ;; D-Bus signature: aa{sv} (array of dict with string keys and variant values)
  ;;  Return an array of dbus vardicts with query-items information.
  ;; This filters / maps the information from `mu4e-query-items'."
  ;; D-Bus signature: aa{sv} (array of dict with string keys and variant values)
  (cons ':array
        (mapcar
         (lambda (plist)
           (let ((dict))
             (while plist
               (let* ((key (car plist)) (val (cadr plist)))
                 (when (memq key '(:name :query :count :unread :favorite))
                   (push (list :dict-entry (substring (symbol-name key) 1) ; eat ':'
                               (list :variant val)) dict))
                 (setq plist (cddr plist))))
             (cons ':array dict)))
         (mu4e-query-items))))

(defun mu4e--dbus-update-query-info ()
  "(Re-)register the query-info property."
  (mu4e--dbus-set-property "QueryInfo" (mu4e--dbus-query-info)))

(defun mu4e--dbus-update-context ()
  "(Re-)register the current mu4e context."
  (let ((ctx (mu4e-context-current)))
    (mu4e--dbus-set-property
     "Context" (if ctx (mu4e-context-name ctx) ""))))

(defun mu4e--dbus-init ()
  "Initialize mu4e's DBus interface."
  (dbus-register-service
   :session mu4e-dbus-service
   :allow-replacement :replace-existing)

  ;; setup introspection
   (mu4e--dbus-introspect-parents)
   (mu4e--dbus-introspect-nl-djcbsoftware-mu4e)

   ;; Register properties
   (mu4e--dbus-register-property "Version" :read mu4e-mu-version)
   (mu4e--dbus-register-property "DatabasePath" :read
                                 (plist-get (mu4e-server-properties) :database-path))
   (mu4e--dbus-register-property "RootMaildir" :read
                                 (plist-get (mu4e-server-properties) :root-maildir))
   (mu4e--dbus-register-property "MessageCounbt" :readwrite
                                 (plist-get (mu4e-server-properties) :doccount))
   (mu4e--dbus-register-property "Context" :readwrite mu4e-mu-version)
   (mu4e--dbus-register-property "QueryInfo" :readwrite (mu4e--dbus-query-info))

   ;; auto-update  context
   (mu4e--dbus-update-context)
   (add-hook 'mu4e-context-changed-hook #'mu4e--dbus-update-context)

   ;; auto-update query info, and update when it changes.
   (mu4e--dbus-update-query-info)
   (add-hook 'mu4e-query-items-updated-hook #'mu4e--dbus-update-query-info))

(defun mu4e--dbus-uninit ()
  "Un-initialize mu4e's DBus interface."
  (ignore-errors (dbus-unregister-service :session mu4e-dbus-service))
  (remove-hook 'mu4e-context-changed-hook #'mu4e--dbus-update-context)
  (remove-hook 'mu4e-query-items-updated-hook #'mu4e--dbus-update-query-info))

;;;###autoload
(define-minor-mode mu4e-dbus-mode
  "Minor mode for enabling and disable the mu4e DBus service."
  :global t
  :group 'mu4e
  :lighter nil
  (unless (featurep 'dbus)
    (mu4e-error "DBus support is required but not found"))
  (if mu4e-dbus-mode
      (mu4e--dbus-init)
    (mu4e--dbus-uninit)))

(provide 'mu4e-dbus)
;;; mu4e-dbus.el ends here
