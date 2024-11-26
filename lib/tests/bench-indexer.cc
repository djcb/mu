/*
** Copyright (C) 2022-2023 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
**
** This program is free software; you can redistribute it and/or modify it
** under the terms of the GNU General Public License as published by the
** Free Software Foundation; either version 3, or (at your option) any
** later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software Foundation,
** Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
**
*/
#include <glib.h>
#include <string>
#include <thread>
#include <vector>
#include <iostream>
#include <fstream>

#include <utils/mu-utils.hh>
#include <utils/mu-regex.hh>
#include <mu-store.hh>
#include "mu-maildir.hh"

#include "utils/mu-test-utils.hh"

using namespace Mu;

constexpr auto test_msg =
R"(Return-Path: <htcondor-users-bounces@cs.wisc.edu>
Received: from pop3.web.de [212.227.17.177]
	by localhost with POP3 (fetchmail-6.4.6)
	for <arne@localhost> (single-drop); Fri, 26 Jun 2020 12:56:08 +0200 (CEST)
Received: from jeeves.cs.wisc.edu ([128.105.6.16]) by mx-ha.web.de (mxweb112
 [212.227.17.8]) with ESMTPS (Nemesis) id 1MdMYE-1jFXaM2gnA-00ZKvt for
 <@ID@@web.de>; Fri, 26 Jun 2020 01:28:11 +0200
Received: from jeeves.cs.wisc.edu (localhost [127.0.0.1])
	by jeeves.cs.wisc.edu (8.14.4/8.14.4) with ESMTP id 05PNLgek013419;
	Thu, 25 Jun 2020 18:22:23 -0500
Received: from shale.cs.wisc.edu (shale.cs.wisc.edu [128.105.6.25])
	by jeeves.cs.wisc.edu (8.14.4/8.14.4) with ESMTP id 05PNLaf0013414
	(version=TLSv1/SSLv3 cipher=AES256-GCM-SHA384 bits=256 verify=OK)
	for <htcondor-users@jeeves.cs.wisc.edu>; Thu, 25 Jun 2020 18:21:36 -0500
Received: from smtp7.wiscmail.wisc.edu (wmmta4.doit.wisc.edu [144.92.197.245])
	by shale.cs.wisc.edu (8.14.4/8.14.4) with ESMTP id 05PNLaMK013694
	(version=TLSv1/SSLv3 cipher=DHE-RSA-AES128-GCM-SHA256 bits=128
	verify=NO)
	for <htcondor-users@cs.wisc.edu>; Thu, 25 Jun 2020 18:21:36 -0500
Received: from USG02-CY1-obe.outbound.protection.office365.us
	([23.103.209.108]) by smtp7.wiscmail.wisc.edu
	(Oracle Communications Messaging Server 8.0.2.4.20190812 64bit (built
	Aug 12
	2019)) with ESMTPS id <0QCI042LC8VUXFC0@smtp7.wiscmail.wisc.edu> for
	htcondor-users@cs.wisc.edu (ORCPT htcondor-users@cs.wisc.edu); Thu,
	25 Jun 2020 18:21:31 -0500 (CDT)
X-Spam-Report: IsSpam=no, Probability=11%, Hits= RETURN_RECEIPT 0.5,
	FROM_US_TLD 0.1, HTML_00_01 0.05, HTML_00_10 0.05, SUPERLONG_LINE 0.05,
	BODYTEXTP_SIZE_3000_LESS 0, BODY_SIZE_10000_PLUS 0, DKIM_SIGNATURE 0,
	KNOWN_MTA_TFX 0, NO_URI_HTTPS 0, SPF_PASS 0, SXL_IP_TFX_WM 0,
	WEBMAIL_SOURCE 0, WEBMAIL_XOIP 0, WEBMAIL_X_IP_HDR 0, __ANY_URI 0,
	__ARCAUTH_DKIM_PASSED 0, __ARCAUTH_DMARC_PASSED 0, __ARCAUTH_PASSED 0,
	__ATTACHMENT_SIZE_0_10K 0, __ATTACHMENT_SIZE_10_25K 0,
	__BODY_NO_MAILTO 0,
	__CT 0, __CTYPE_HAS_BOUNDARY 0, __CTYPE_MULTIPART 0, __HAS_ATTACHMENT 0,
	__HAS_ATTACHMENT1 0, __HAS_ATTACHMENT2 0, __HAS_FROM 0, __HAS_MSGID 0,
	__HAS_XOIP 0, __HIGHBITS 0, __MIME_TEXT_P 0, __MIME_TEXT_P1 0,
	__MIME_TEXT_P2 0, __MIME_VERSION 0, __MULTIPLE_RCPTS_TO_X2 0,
	__NO_HTML_TAG_RAW 0, __RETURN_RECEIPT_TO 0, __SANE_MSGID 0,
	__TO_MALFORMED_2 0, __TO_NAME 0, __TO_NAME_DIFF_FROM_ACC 0,
	__TO_NO_NAME 0,
	__TO_REAL_NAMES 0, __URI_IN_BODY 0, __URI_MAILTO 0, __URI_NOT_IMG 0,
	__URI_NO_PATH 0, __URI_NS , __URI_WITHOUT_PATH 0
X-Wisc-Doma: @ID@X@numerica.us,numerica.us
X-Wisc-Env-From-B64: d2VzbGV5LnRheWxvckBudW1lcmljYS51cw==
X-Spam-PmxInfo: Server=avs-13, Version=6.4.7.2805085,
	Antispam-Engine: 2.7.2.2107409, Antispam-Data: 2020.6.25.231519,
	AntiVirus-Engine: 5.74.0, AntiVirus-Data: 2020.6.25.5740002,
	SenderIP=[23.103.209.108]
X-Wisc-DKIM-Verify: @ID@XXXXXXX@numerica.us,numericaus.onmicrosoft.com!pass
X-Spam-Score: *
ARC-Seal: i=1; a=rsa-sha256; s=arcselector5401; d=microsoft.com; cv=none;
	b=KyXoddJsnsHsBwhdlO5rcljgMRaylJAUAxWTjG4jQL1C8XJAMgeERtH2sRffdjibYUFfSuDUNJmrTrvrbjKGUt2I8J2M2MgUB/upMoroVPNBrP1Fy9wMeZJQuSS4r4KjZZktsl2i8eq667pzOZO6+wX2IA5M7YtxDqglcWOE6btWzbABVjx+9eCXMt0eMd1+UI6ABK8Frd33EFQLKT0h/cxidWR9l+0gCMAcRxsLrQ82+ckU606AIV/DA1E4Tq7ADe/+CRv4QszDN93pWL/1N2/OOh9vFTs9g9ZG6uXjN+Km/IAdylPbfHgKW60ev3/Bvv6N3pA7DjpuiKj6BnW7mQ==
ARC-Message-Signature: i=1; a=rsa-sha256; c=relaxed/relaxed; d=microsoft.com;
	s=arcselector5401;
	h=From:Date:Subject:Message-ID:Content-Type:MIME-Version:X-MS-Exchange-SenderADCheck;
	bh=OZrj1we1ZUH0xBMhJ5/F6EQnB0cmitFs2xZW1fLMRNs=;
	b=Pq07a3u26s2UdpucJuVQ0h68272wx46Wp61x/30TelPPFLCRxVjmlH1U3IBmIsZ1jOEtGXFJRv65L3HmwGxRUdLlMOdPRB64BBfHQ9NGWUBykKQmOrJNGJs635nEdpugpzngzIdcg1PS5vHxPJAnOeqoo71OVPI3JqPrPEn2TJJgb9J6PApexkqIbVl35prGPsyS/t2IlYw3/ihWzORG6wvqJeqedgpJTBXeGaDoMa+MQ1BeUsdvybh8+hau4ASpM5lwyeXlGmJ5mUTZi39jp+dFdDrmCj/VM4ezeuXeH9+HFtDjKLZJaTDWUID0IBcr91BaoQE/4r6y+lpkah6LLQ==
ARC-Authentication-Results: i=1; mx.microsoft.com 1; spf=pass
	smtp.mailfrom=numerica.us;
	dmarc=pass action=none header.from=numerica.us;
	dkim=pass header.d=numerica.us; arc=none
DKIM-Signature: v=1; a=rsa-sha256; c=relaxed/relaxed;
	d=numericaus.onmicrosoft.com; s=selector1-numericaus-onmicrosoft-com;
	h=From:Date:Subject:Message-ID:Content-Type:MIME-Version:X-MS-Exchange-SenderADCheck;
	bh=OZrj1we1ZUH0xBMhJ5/F6EQnB0cmitFs2xZW1fLMRNs=;
	b=cFn0eL5k2IKry9U8qa8mbVaxRiyicUAWzRc3NUtj+VEbgShfrz8SO6FPX20WTQQJg/Fu/3isqsSEUt+9NSEEbgd5eQ1EVz5E/JVeNjPe9GXR0JEF/g3f6yM7CO+kKTvXSRvQjce683U0j7Aj1pSDEktoVNP4xvOS2Gx9VjdWTmc=
Received: from DM3P110MB0474.NAMP110.PROD.OUTLOOK.COM (2001:489a:200:413::10)
	by DM3P110MB0490.NAMP110.PROD.OUTLOOK.COM (2001:489a:200:413::14)
	with Microsoft SMTP Server
	(version=TLS1_2, cipher=TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384)
	id 15.20.3109.25; Thu, 25 Jun 2020 23:21:07 +0000
Received: from DM3P110MB0474.NAMP110.PROD.OUTLOOK.COM
	([fe80::f548:f084:9867:9375]) by DM3P110MB0474.NAMP110.PROD.OUTLOOK.COM
	([fe80::f548:f084:9867:9375%11]) with mapi id 15.20.3131.024; Thu,
	25 Jun 2020 23:21:07 +0000
From: Raul Endymion <XXXXXXXXXXXXXXXXXXXX@numerica.us>
To: "'htcondor-users@cs.wisc.edu'" <htcondor-users@cs.wisc.edu>
Thread-topic: OPINIONS WANTED: Are there any blatent downsides I am missing to
	the following Condor configuration
Thread-index: AdZLRbEvYoEDBZChS62aOHgPzKD8kw==
Date: Thu, 25 Jun 2020 23:21:06 +0000
Message-id: <DM3P110MB04746CDBA55B3E597EFD1877FA920@DM3P110MB0474.NAMP110.PROD.OUTLOOK.COM>
Accept-Language: en-US
Content-language: en-US
X-MS-Has-Attach: yes
X-MS-TNEF-Correlator:
X-Originating-IP: [50.233.29.54]
x-ms-publictraffictype: Email
x-ms-office365-filtering-correlation-id: f4edecdf-5582-4b2d-226a-08d8195e7007
x-ms-traffictypediagnostic: DM3P110MB0490:
x-microsoft-antispam-prvs: <DM3P110MB04907C313C4243B4FE42A20CFA920@DM3P110MB0490.NAMP110.PROD.OUTLOOK.COM>
x-ms-oob-tlc-oobclassifiers: OLM:10000;
x-forefront-prvs: 0445A82F82
x-ms-exchange-senderadcheck: 1
x-microsoft-antispam: BCL:0;
X-Forefront-Antispam-Report: CIP:255.255.255.255; CTRY:; LANG:en; SCL:1; SRV:;
	IPV:NLI; SFV:NSPM; H:DM3P110MB0474.NAMP110.PROD.OUTLOOK.COM;
	PTR:; CAT:NONE; SFTY:;
	SFS:(346002)(366004)(6916009)(83380400001)(71200400001)(8936002)(55016002)(5660300002)(8676002)(9686003)(66616009)(64756008)(33656002)(52536014)(66446008)(66476007)(66556008)(66946007)(99936003)(76116006)(186003)(86362001)(26005)(7696005)(6506007)(44832011)(508600001)(2906002)(80162005)(80862006)(491001)(554374003);
	DIR:OUT; SFP:1102;
x-ms-exchange-transport-forked: True
MIME-version: 1.0
X-OriginatorOrg: numerica.us
X-MS-Exchange-CrossTenant-Network-Message-Id: f4edecdf-5582-4b2d-226a-08d8195e7007
X-MS-Exchange-CrossTenant-OriginalArrivalTime: 25 Jun 2020 23:21:06.8341 (UTC)
X-MS-Exchange-CrossTenant-fromentityheader: Hosted
X-MS-Exchange-CrossTenant-id: fae7a2ae-df1d-444e-91be-babb0900b9c2
X-MS-Exchange-CrossTenant-mailboxtype: HOSTED
X-MS-Exchange-Transport-CrossTenantHeadersStamped: DM3P110MB0490
Subject: [HTCondor-users] OPINIONS WANTED: Are there any blatent downsides I
 am missing to the following Condor configuration
X-BeenThere: htcondor-users@cs.wisc.edu
X-Mailman-Version: 2.1.19
Precedence: list
List-Id: HTCondor-Users Mail List <htcondor-users.cs.wisc.edu>
List-Unsubscribe: <https://lists.cs.wisc.edu/mailman/options/htcondor-users>,
	<mailto:htcondor-users-request@cs.wisc.edu?subject=unsubscribe>
List-Archive: <https://www-auth.cs.wisc.edu/lists/htcondor-users/>
List-Post: <mailto:htcondor-users@cs.wisc.edu>
List-Help: <mailto:htcondor-users-request@cs.wisc.edu?subject=help>
List-Subscribe: <https://lists.cs.wisc.edu/mailman/listinfo/htcondor-users>,
	<mailto:htcondor-users-request@cs.wisc.edu?subject=subscribe>
Reply-To: HTCondor-Users Mail List <htcondor-users@cs.wisc.edu>
Content-Type: multipart/mixed; boundary="===============0678627779074767862=="
Errors-To: htcondor-users-bounces@cs.wisc.edu
Sender: "HTCondor-users" <htcondor-users-bounces@cs.wisc.edu>
Envelope-To: <@ID@XXXXX@web.de>
X-UI-Filterresults: unknown:2;V03:K0:cdojl5YHfkg=:jhTbQXp38SL2za/LB4M7aUwpyw
 5rDHoN1+/ScH/O9/G1fKWbGryQ203thF+1ZrHUOOwq8MVOc5SsoqzSTsaNbEAdthFcDDz3Oui
 SHxX1hdpV3UOjZEHzWlpjEjRe7t74g2RI/ESELmkPuLg/LZC7SjAsg70cTJBIfDPYxJkJAcUl
 9W6OEBsmtTDO0va/EQRYjfkpoF9tjfmfMNw9KSKHuDdqZu2Xfak8mQKnWsoxWeUkD31r60iPC
 yikbj7KP5AlHaWMzyTTdlvtjYRLfSuUSe1uqjI5NWCnZDDjz7zODoaWPp7p2U/MQenXEjN6+M
 WnZL6ZC8AGtze/hYgOCXcLf4ydQ7m9YueJiY5nDn7g+cwnhxypVNFTL5NjSpKKXbkzbyu9Tdl
 ez+92g/9pGW17iOo5NrFtfctLlmCEH0RxjouKI7FBmv3bIvFC4FvfghiNf7OZmRg2/nT5i+1o
 AICYNAx2y5CezKsKM2f1tm60dkydQIR8pK45dDKZPz3i7NeJm9dknZ2OYFTnucUvdPaT8nR43
 cK3kk2QUE48Ngo/0NwepSGrV9TkOt+hY3PUYkXWp/mwP2QPSjy4cALyvLyKwG24qZ9CiiRLMV
 KqPFlCRnoDG5MHJ4d0krFlqmg8rNsWzV3oWfMNKFZmD24lVUmWGb+ExxbCFc0xzIt12o/EqBw
 nVkXLu+E0apM+cmG6ubYfOymRoUpiKsZI9ivc+mAaEE+v2RBzcAURzlhzQHIn81onvzbQwCge
 tMtBkSEfqyoa1HjalX4B9WQ90M42K+7xW039ydakQ7JOeYVpkPXYoBF7mbrXRckhMXjQatLQ8
 MWA8+U031Xfa1ueOIfCCkzJ49wyx1LoLPyqdjCvnzaRd72yNEMJ5zM/itMIPE9reIHBtpom0i
 RhIYdJFDrL+SKqE68lcJakCcF3R+VLApwLKOr0HChGQjdEk7c/rm5E0dF5f3oYlHf591QoXIJ
 h16yfcJYe6fMo1YYunkvbEDFPpzttIq7aIk0FzxrOdRvj3yQajDbwOpYI/5T/DaabPn3M8lK7
 8pn7LrbmyCaLHhkYMS4h3SDkYWsifza6vkldizrK7IPf6KhS7AhTkbnEonWS6454GLUg1nYGX
 W5Qp/G9LzvjtEGQMcwnCN5jb5zq7o3f+9FrROKjpwFxL+mL85CEXY/KMOVpf6hDJJfSyu6/X6
 FpbwlJVLFdGeA0/+xcKcmutpkJACgK2kHqvZ8MZxt+5jBJWVlIDLZKa8/IoGWC+ikLX2/hPNB
 4TU89QYG5ygPmwwDXruFG7N7jVURZceHqWNKtqegS6YQ5nirsPJWJR7jzgr+HbntUaQETXNpn
 QrxpsVHXfqRu2GlP5h28RaIpvBVUcwqrs+eLJELStvBzyAmaVPVoKFjEWFfwrmE89W6Bmz2W3
 kHExOq3hI3gDsGXKjTjT/kjHkaHmtnVUXr4vqovf8Ht4Vwmtf0S4xsgpYjnYjUIzG9eiwIFAZ
 hL2gvjwW51qtMvybf01C50xTiS9GSfO0SR7meBPA67skcA+wFo11wmwXsUk1irpKnC+Y9hVZX
 2vPkfZ1T2VXNo997cQC59lBpi/TU5gnuM7H/Vcl7tF3Lqtmqut7s6HkPWCegDZ3O2W7shH7aZ
 1bOXbO+W/SNC+WcMnj+fhuP+dHcrt0Vw4RD9knJOOzdZTH3OCli/vpjqgTbCKEaWMhCIeM2g0
 RiLFxTeTEEBCa49bwa8n2r4T/vA3duZd8F/DNKvWTfhRr1Mxtz3n15EOar13fFijtnieEiv4/
 vO/5uRF+H86Fcoua7B8AswThbiG1vou6M48g0Zo6iGEcrueKEaHMI4XM7wQF77KazMdn5f1BP
 +KyQX83aHJN/qGniXgF8yu+h0M7Nf0YrTteYQd2C/HZrIA8IaLqqvLoGRl7dRBnbZiP7jRdQm
 1YEYtjX4XBoShrXPfIxPnJBUBnnOaePYxOJkS2FaBv19jPkMnyc9xuJYD7JOTFnXKzAnoaBqT
 OR+dGrLLGZ1MM/0gqclKTv7Hcce+6CJyTWkx5mq42w49HFI/kdHBRxU8xIRv4B9l0ePf9EbWr
 cDcrssee//6KXiRmF4fm7jq828/uhj8MIJet9sIU5ncKwHEse3I4YmVT5+dB+ZGZh0gbJPFj6
 xcICpshhYct+euMCdNfy3lkxiRr76RwfBzLAOP5+1U3GAx/hcsL2AgyBHMwWo+Kkeq8pPy4YI
 pQMxJyylI6JMa/DbBggnDk+xNZpRKo/XA4lAJY57DCOPL2ZcL8kU2aCd5LjtYHK0ZWSFtOjxs
 oIEr/f2vvg+zibxzaANBzylZn3yPe9pI/IBefu9fL4MVaYY3aboxuncX4fyi0VH0WbFkSYXRi
 a7LIu3LI2LTU13C/LE7j9hmxP6TApyiXi14f0GSa2sbF6HWp2v2rhYM7h67AAn3SQgvcJLpgb
 Hz5ABb/OAk6ABVEl+a483zexJ6iT2P0gYc08zmewy8Jf8AD9r846k9pGZuhBaOHREx3bA16Bj
 uWYh3QzSI6MQoJM3XbBGLVkX36Lfj54T9kk97lLaxfbGPuNoyOV9iTBKxts3m2KD+52iH3EEi
 glbH6HNIUHyCHdEXsXyGVFwfM9V7OQcVO/g266KIQ74wU16x/Zdsq4p/1PcRXHRnoMxP/pUrj
 EOLWzFU71qzC/OSkYWRil9HXUyucTFGQ0N08jZNXctI9lElWtgq3iI+Cz2F20rz+LJGhSHSkZ
 0G5JgXrtspeJN5yoH6TOE0hblr5sZcAM0wiSP7x/hPBeYHswzTA5/laWMn++9aTPVgpPaJ9/x
 wyLm55OZr4Jl+StWd3MqLCgiRB3cNGrDX7f8Eqnj4wfCHiGIUHewD4qrfXraZQhIk17W+9JyD
 osmUiVD9ZRdNCY2eNnu8ZkJ4uzKl44lwLL43sInKBjdAHlnoxrR2FOrYXbnU31ujwxdeUr6Hs
 xPFy0Git0CpWCWYmaz37KA8GW7PE4ffWzcfCmz6AKBrbHcCreeUnyqnSEDy9ubnz7mcLRnu3W
 RAWi6diI8gcS9g0+r4z5PtZX9rveXRekHJ4k08VuYVmdiz3gjXmHPlm9IKPEAbygP2EYgjwGE
 RbReLc8xHJlfLbwdXyGw0HU=

--===============0678627779074767862==
Content-language: en-US
Content-type: multipart/signed; protocol="application/x-pkcs7-signature";
 micalg=2.16.840.1.101.3.4.2.3;
 boundary="----=_NextPart_000_0018_01D64B14.F58791A0"

------=_NextPart_000_0018_01D64B14.F58791A0
Content-Type: text/plain;
	charset="utf-8"
Content-Transfer-Encoding: quoted-printable

Hey!

I am architecting our final HTCondor configuration over here and I have =
an idea I am unsure about and I would like to ask some experienced users =
for their opinion.

Background, we have a small, relatively homogenous cluster (with no =
special universes) and less than 10 users. Since each user has their own =
workstation separate from our cluster I thought the following =
configuration would suit our needs, but I want to make sure there isn't =
a huge disadvantage I am missing:

1. Set the Central Manager to be highly available to the point of =
tolerating N cluster machine failures
2. Put a Submit on each of the users' workstations (I am a little =
worried about the resource usage of the condor_shadow and condor_schedd, =
my users are already running into RAM consumption issues over time as it =
is)
3. Place an Execute on each of the cluster machines, which would lead to =
the central manager being on a machine that is also executing jobs

Fortunately both my users' and cluster machines all have access to the =
same network storage, and we have centralized authentication so we can =
just use our users' credentials to authenticate everywhere.=20

Before I set this in dry mud, does anyone have any retrospective =
recommendations I could benefit hearing from, since I am still pretty =
new to the project?

Thank you!
-Raul

Raul Endymion =E2=80=93 Cluster Manager
Numerica Corporation (www.numerica.us)
5042 Technology Parkway #100
Fort Collins, Colorado 80528
=E2=98=8E=EF=B8=8F (970) 207 2233
=F0=9F=93=A7 @ID@XXXXXXXXXXXXXXX@numerica.us



------=_NextPart_000_0018_01D64B14.F58791A0
Content-Type: application/pkcs7-signature;
	name="smime.p7s"
Content-Transfer-Encoding: base64
Content-Disposition: attachment;
	filename="smime.p7s"

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
V/zEGk/PEc3pZxNAo7m0I/SXdNulUEkjxai5Wq53i0EhWVLpUU8XY3joXax46yCMqh0PUn90QmMD
BybLyFDX6av8tVS5cQs0HbTZdIuj7A/dsKzKKIrSHd3SQ9MLNPRkSRdhagmf5LCF1Y4xEEiuAA/H
XdYAxGIcl8n6b2CcLlZzq4W13Ipv8FIZoDsG1u0b9NGfeSOOHidi5kdD6r8lM5PaSPmZsl5PdKK6
+E1Y6rBCvITu0MBo5Tjuwt5cok3Ve0BK5Fg89aIL2/rMicm20qG6nbqxLhHeR0mhPO98KIIzDoeL
rLpAlWS7GoPvJqbRzxsCAwEAAaOCAlYwggJSMBAGCSsGAQQBgjcVAQQDAgEBMCMGCSsGAQQBgjcV
AgQWBBSv5TU1Bjnw5n3u1iO2y+BHQXk7MTAdBgNVHQ4EFgQUoeMyqBhiyBcgwJN8zbr7pRbgs+sw
GQYJKwYBBAGCNxQCBAweCgBTAHUAYgBDAEEwDgYDVR0PAQH/BAQDAgGGMA8GA1UdEwEB/wQFMAMB
Af8wHwYDVR0jBBgwFoAUF+CLMX/eZk96ElRSeiEHqnsujqEwgdMGA1UdHwSByzCByDCBxaCBwqCB
v4aBvGxkYXA6Ly8vQ049YWQtR0lMR0FMQUQtQ0EsQ049R2lsZ2FsYWQsQ049Q0RQLENOPVB1Ymxp
YyUyMEtleSUyMFNlcnZpY2VzLENOPVNlcnZpY2VzLENOPUNvbmZpZ3VyYXRpb24sREM9YWQsREM9
bnVtZXJpY2EsREM9dXM/Y2VydGlmaWNhdGVSZXZvY2F0aW9uTGlzdD9iYXNlP29iamVjdENsYXNz
PWNSTERpc3RyaWJ1dGlvblBvaW50MIHGBggrBgEFBQcBAQSBuTCBtjCBswYIKwYBBQUHMAKGgaZs
ZGFwOi8vL0NOPWFkLUdJTEdBTEFELUNBLENOPUFJQSxDTj1QdWJsaWMlMjBLZXklMjBTZXJ2aWNl
cyxDTj1TZXJ2aWNlcyxDTj1Db25maWd1cmF0aW9uLERDPWFkLERDPW51bWVyaWNhLERDPXVzP2NB
Q2VydGlmaWNhdGU/YmFzZT9vYmplY3RDbGFzcz1jZXJ0aWZpY2F0aW9uQXV0aG9yaXR5MA0GCSqG
SIb3DQEBCwUAA4ICAQBmRoSlPe++k7tsAJOvq0+0dNI6yk6gOBmY4g5jL9NTEjSxPWkeYegIwLr2
UqpiIIZmAh9e9v3z0T2egVyRqNezLPXLkg/2gUfV6D0kRyKtG5mL0yAn/0hkkVyf6jWJpCKmH77x
0w3UpnfKs79jv5YpQDhC2eRFivN50HhIkigLWScPq4zd81ghmN8VFTHVQmsGua/mm1Oj5/pBFuQF
B4ljon1N//wX5ZJZaUlJR9eR9tM9m+Gyds2flr5+mZT6Zgm26fKiC5zs91aGnzqGx6s30jfXELP2
FjFrrR46ooV7ehhnyBlCACxIWqXe5sSZsSh9oEYZ7Ux5Vq0thkfArBWsF7HA+LovKCUyHLcXbVBB
6/VAwZ3GLYi/bqbVIEFlVRu4nv/JyKWwoGbAhGyzZNWoeHszFrEIQbQMoMsEumVkMZreE6AxP+zb
6JPPOjlhpymtMo54z1MDYJPyo4HmcpL4xUjHZgqgOxMrbHC4oIVLvKZ/scbVBhPnd0tHHSZqj3ZS
gfTvG/ut/tLNTXXe48PkLBw4KguhbLm61Elu3wJALT0UL+ENgUWwb7csUGQBqOyPAHXGYnf/ACOc
UBqQckcrK8Jq3u8rnCloW3uDw86hw7MFM+YjmhVRdYRxpJmhKVPT6Amufp2WsSVId8q3CSqTH33L
fcxbV1n7hLWHA67MhTCCBq0wggWVoAMCAQICEycAAAsJMaw2RjtHZFUAAQAACwkwDQYJKoZIhvcN
AQELBQAwXDESMBAGCgmSJomT8ixkARkWAnVzMRgwFgYKCZImiZPyLGQBGRYIbnVtZXJpY2ExEjAQ
BgoJkiaJk/IsZAEZFgJhZDEYMBYGA1UEAxMPYWQtQ0VMRUJSSUFOLUNBMB4XDTIwMDUxMjE1MDk0
MloXDTIxMDcyMjE0MjgwMVowgcExEjAQBgoJkiaJk/IsZAEZFgJ1czEYMBYGCgmSJomT8ixkARkW
CG51bWVyaWNhMRIwEAYKCZImiZPyLGQBGRYCYWQxETAPBgNVBAsTCE51bWVyaWNhMQ4wDAYDVQQL
EwVVc2VyczEYMBYGA1UECxMPUHJlc2VudCBJbnRlcm5zMRYwFAYDVQQDEw1XZXNsZXkgVGF5bG9y
MSgwJgYJKoZIhvcNAQkBFhl3ZXNsZXkudGF5bG9yQG51bWVyaWNhLnVzMIIBIjANBgkqhkiG9w0B
AQEFAAOCAQ8AMIIBCgKCAQEA5clDLapXkiLVXhAFP9GJv+JJkt+cacyvWaX9xEvqMQXOXb7MqO5E
DJE8XPMfxaX84WhuMMePOc9SNUKpDtTa2SHz+AOom+JH38ce2gfrdOPwez/e6RrUb3o8ZvMr3hJl
Yy+6vEFEADIICfHSlIjkLJbGNFTRDccvkOPjD2W+fmzFAtWyNb/eqM+mwdTuXjOxTvP6V34zJsvc
YKJUzhhD8jI7GdqOoNoirTlaMVTH5udK0P2KvzD6F0LfwcOlc3bTvY9uI585xhdniK4yAIka8OMq
5zmyEQLYOadcVSscjAlkC1sQ0gbwL3AdwS+bntryq+2Ds380OJ+Z1Uy7TRkeBQIDAQABo4IDADCC
AvwwPAYJKwYBBAGCNxUHBC8wLQYlKwYBBAGCNxUI9/Bss4wDhbmBGISeqheH4YBfgSWC6qJEgcjE
IgIBZQIBKDATBgNVHSUEDDAKBggrBgEFBQcDBDAOBgNVHQ8BAf8EBAMCBaAwGwYJKwYBBAGCNxUK
BA4wDDAKBggrBgEFBQcDBDBEBgkqhkiG9w0BCQ8ENzA1MA4GCCqGSIb3DQMCAgIAgDAOBggqhkiG
9w0DBAICAIAwBwYFKw4DAgcwCgYIKoZIhvcNAwcwHQYDVR0OBBYEFDZHoDwoOKD5uzpF/2CcZSeg
XWLmMB8GA1UdIwQYMBaAFKHjMqgYYsgXIMCTfM26+6UW4LPrMIHVBgNVHR8Egc0wgcowgceggcSg
gcGGgb5sZGFwOi8vL0NOPWFkLUNFTEVCUklBTi1DQSxDTj1DZWxlYnJpYW4sQ049Q0RQLENOPVB1
YmxpYyUyMEtleSUyMFNlcnZpY2VzLENOPVNlcnZpY2VzLENOPUNvbmZpZ3VyYXRpb24sREM9YWQs
REM9bnVtZXJpY2EsREM9dXM/Y2VydGlmaWNhdGVSZXZvY2F0aW9uTGlzdD9iYXNlP29iamVjdENs
YXNzPWNSTERpc3RyaWJ1dGlvblBvaW50MIHHBggrBgEFBQcBAQSBujCBtzCBtAYIKwYBBQUHMAKG
gadsZGFwOi8vL0NOPWFkLUNFTEVCUklBTi1DQSxDTj1BSUEsQ049UHVibGljJTIwS2V5JTIwU2Vy
dmljZXMsQ049U2VydmljZXMsQ049Q29uZmlndXJhdGlvbixEQz1hZCxEQz1udW1lcmljYSxEQz11
cz9jQUNlcnRpZmljYXRlP2Jhc2U/b2JqZWN0Q2xhc3M9Y2VydGlmaWNhdGlvbkF1dGhvcml0eTBS
BgNVHREESzBJoCwGCisGAQQBgjcUAgOgHgwcd2VzbGV5LnRheWxvckBhZC5udW1lcmljYS51c4EZ
d2VzbGV5LnRheWxvckBudW1lcmljYS51czANBgkqhkiG9w0BAQsFAAOCAQEAX3zFhiDYU+vQap2J
hiysyC9L7nkL7VI2OQWg4Z/JnNJTFiA6BwtoDYAT4qq1Jix4hZc+g78Gj99OnkhlBQDe9Hq12yI9
muboQSDAYO6iDK76wQv3Rt8Fl4SUD4Ygwy52QrkTDrj/HZxTNask5p/2ilGBJnG9KT2VbEgGJkP9
kXn1vAgOl3BCxgjdWekWCvxpmffr+Z3UtmQIiZAB3OsKcgdsSy9pveTMjxtKJemaH3kpXQiTgCev
CMuWZb3YnqXI8Fd+uUw6HwA4c+ZH62G9Q8KGkwXyhOPizmm3UeSlMo27yUCE+cF5EIHBxpGJ6z83
7MbxMVKnS1Wz1n8MtW2ezDGCBCEwggQdAgEBMHMwXDESMBAGCgmSJomT8ixkARkWAnVzMRgwFgYK
CZImiZPyLGQBGRYIbnVtZXJpY2ExEjAQBgoJkiaJk/IsZAEZFgJhZDEYMBYGA1UEAxMPYWQtQ0VM
RUJSSUFOLUNBAhMnAAALCTGsNkY7R2RVAAEAAAsJMA0GCWCGSAFlAwQCAwUAoIICfzAYBgkqhkiG
9w0BCQMxCwYJKoZIhvcNAQcBMBwGCSqGSIb3DQEJBTEPFw0yMDA2MjUyMzIwNDRaME8GCSqGSIb3
DQEJBDFCBEBaj66vdgjAhEO0p7lO6X44h+LpUlAcROa5Hi4Jp5aWS4hU8CuqOrH12y2GRNmNhKLa
0YieL4fCL3YqDRfop79NMFIGCyqGSIb3DQEJEAIBMUMwQQQdAAAAABAAAACgLzslsB99TKIYKeHy
Wh5cAQAAAACAAQAwHTAbgRl3ZXNsZXkudGF5bG9yQG51bWVyaWNhLnVzMIGCBgkrBgEEAYI3EAQx
dTBzMFwxEjAQBgoJkiaJk/IsZAEZFgJ1czEYMBYGCgmSJomT8ixkARkWCG51bWVyaWNhMRIwEAYK
CZImiZPyLGQBGRYCYWQxGDAWBgNVBAMTD2FkLUNFTEVCUklBTi1DQQITJwAACwkxrDZGO0dkVQAB
AAALCTCBhAYLKoZIhvcNAQkQAgsxdaBzMFwxEjAQBgoJkiaJk/IsZAEZFgJ1czEYMBYGCgmSJomT
8ixkARkWCG51bWVyaWNhMRIwEAYKCZImiZPyLGQBGRYCYWQxGDAWBgNVBAMTD2FkLUNFTEVCUklB
Ti1DQQITJwAACwkxrDZGO0dkVQABAAALCTCBkwYJKoZIhvcNAQkPMYGFMIGCMAsGCWCGSAFlAwQB
KjALBglghkgBZQMEARYwCgYIKoZIhvcNAwcwCwYJYIZIAWUDBAECMA4GCCqGSIb3DQMCAgIAgDAN
BggqhkiG9w0DAgIBQDALBglghkgBZQMEAgMwCwYJYIZIAWUDBAICMAsGCWCGSAFlAwQCATAHBgUr
DgMCGjANBgkqhkiG9w0BAQEFAASCAQBNFxhcbK6Rmw0Xyu+79cH5kUsXENcdUaJPKlegcY/gl2BZ
0CPpGcRnwz6z8OPYjvw3jrkiAE8nBbuCKu1CPtuk1h4Cybk7exyMybYvK5xge+N+dz2mFipRfGSY
rl/ztX1jyvcDruxaSJwb8WMhAGs505yfaCJfwgFOI3QGi+wUunbOIKy3QQZTXDv89yslZqi0wmeI
8sVRqSAYZRIPEylwS9CU2ReK9BJlfVLZnNP1At4gHE6S2hk8T0eVeLT8uhQiUXXJe4644UoPhoA4
Fxgm7Q62KT6yP9O7c4eZzmQ4A9hdlWM6CtZ5pgMAzLOrVFdypzSc+S1j8DqcFkALCw83AAAAAAAA

------=_NextPart_000_0018_01D64B14.F58791A0--

--===============0678627779074767862==
Content-Type: text/plain; charset="us-ascii"
MIME-Version: 1.0
Content-Transfer-Encoding: 7bit
Content-Disposition: inline

_______________________________________________
HTCondor-users mailing list
To unsubscribe, send a message to htcondor-users-request@cs.wisc.edu with a
subject: Unsubscribe
You can also unsubscribe by visiting
https://lists.cs.wisc.edu/mailman/listinfo/htcondor-users

The archives can be found at:
https://lists.cs.wisc.edu/archive/htcondor-users/
--===============0678627779074767862==--)";

static std::string
message(const Regex& rx, size_t id)
{
	char buf[16];
	::snprintf(buf, sizeof(buf), "%zu", id);

	return to_string_gchar(
		g_regex_replace(rx, test_msg, -1, 0, buf,
				G_REGEX_MATCH_DEFAULT, {}));
}

struct TestData {
	size_t num_maildirs;
	size_t num_messages;
	size_t num_threads;
};


static void
setup(const TestData& tdata)
{
	/* create toplevel */
	auto top_maildir = std::string{BENCH_MAILDIRS};
	int res = g_mkdir_with_parents(top_maildir.c_str(), 0700);
	g_assert_cmpuint(res,==, 0);

	/* create maildirs */
	for (size_t i = 0; i != tdata.num_maildirs; ++i) {
		const auto mdir = mu_format("{}/maildir-{}", top_maildir, i);
		auto res = maildir_mkdir(mdir);
		g_assert(!!res);
	}
	const auto rx = Regex::make("@ID@");
	/* create messages */
	for (size_t n = 0; n != tdata.num_messages; ++n) {
		auto mpath = mu_format("{}/maildir-{}/cur/msg-{}:2,S",
				    top_maildir, n % tdata.num_maildirs,
				    n);
		std::ofstream stream(mpath);
		auto msg = message(*rx, n);
		stream.write(msg.c_str(), msg.size());
		g_assert_true(stream.good());
	}
}

static void
tear_down()
{
	/* ugly */
	GError *err{};
	const auto cmd{mu_format("/bin/rm -rf '{}' '{}'", BENCH_MAILDIRS, BENCH_STORE)};
	if (!g_spawn_command_line_sync(cmd.c_str(), NULL, NULL, NULL, &err)) {
		mu_warning("error: {}", err ? err->message : "?");
		g_clear_error(&err);
	}
}

void
black_hole(void)
{
	return; /* do nothing */
}

static void
benchmark_indexer(gconstpointer testdata)
{
	using namespace std::chrono_literals;
	using Clock = std::chrono::steady_clock;
	const auto tdata = reinterpret_cast<const TestData*>(testdata);

	setup(*tdata);
	auto start = Clock::now();

	{
		auto store{Store::make_new(BENCH_STORE, BENCH_MAILDIRS)};
		g_assert_true(!!store);
		Indexer::Config conf{};

		auto res = store->indexer().start(conf);
		g_assert_true(res);
		while(store->indexer().is_running()) {
			std::this_thread::sleep_for(100ms);
		}
		g_assert_cmpuint(store->size(),==, tdata->num_messages);
	}

	const auto elapsed = Clock::now() - start;
	std::cout << "indexed " << tdata->num_messages << " messages in "
		  << tdata->num_maildirs << " maildirs in "
		  << to_ms(elapsed) << "ms; "
		  << to_us(elapsed) / tdata->num_messages << " μs/message; "
		  << static_cast<size_t>(1000*tdata->num_messages / to_ms(elapsed))
		  << " messages/s"
		  << " (" << tdata->num_threads << " thread(s))\n";

	tear_down();
}

int
main(int argc, char *argv[])
{
	size_t num_maildirs{}, num_messages{};

	mu_test_init(&argc, &argv);

	if (g_test_perf()) {
		num_maildirs = 20;
		num_messages = 5000;
	} else {
		num_maildirs = 10;
		num_messages = 1000;
	}

	g_log_set_handler(
	    NULL,
	    (GLogLevelFlags)(G_LOG_LEVEL_MASK | G_LOG_FLAG_FATAL | G_LOG_FLAG_RECURSION),
	    (GLogFunc)black_hole,
	    NULL);


	size_t thread_num{};
	const auto tnum = g_getenv("THREAD_NUM");
	if (tnum)
		thread_num = ::strtol(tnum, NULL, 10);

	if (thread_num != 0) {
		/* THREAD_NUM specified */
		static TestData tdata{num_maildirs, num_messages, thread_num};
		char *name = g_strdup_printf("/bench/indexer/%zu-cores", thread_num);
		g_test_add_data_func(name, &tdata, benchmark_indexer);
		g_free(name);
	} else {
		/* no THREAD_NUM specified */

		const size_t hw_threads = std::thread::hardware_concurrency();

		{
			static TestData tdata{num_maildirs, num_messages, 1};
			g_test_add_data_func("/bench/indexer/1-core", &tdata, benchmark_indexer);
		}

		if (hw_threads > 2) {
			static TestData tdata{num_maildirs, num_messages, hw_threads/2};
			char *name = g_strdup_printf("/bench/indexer/%zu-cores", hw_threads/2);
			g_test_add_data_func(name, &tdata, benchmark_indexer);
			g_free(name);
		}

		if (hw_threads > 1) {
			static TestData tdata{num_maildirs, num_messages, hw_threads};
			char *name = g_strdup_printf("/bench/indexer/%zu-cores", hw_threads);
			g_test_add_data_func(name, &tdata, benchmark_indexer);
			g_free(name);
		}
	}

	tear_down();

	return g_test_run();
}
