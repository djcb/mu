/* 
** Copyright (C) 2010 Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 3 of the License, or
** (at your option) any later version.
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

#if HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/


#include <glib.h>
#include <string.h>
#include <ctype.h>

#include "mu-str.h"

char*
mu_str_normalize (const char *str, gboolean downcase)
{
	g_return_val_if_fail (str, NULL);

	return mu_str_normalize_in_place (g_strdup(str), downcase);
}

/* we can normalize in-place, as the normalized string will never be
 * longer than the original.  even for replacements that are 2 chars
 * wide (e.g. German ß => ss), the replacement is 2 bytes, like the
 * original 0xc3 0x9f
 */
char*
mu_str_normalize_in_place (char *str, gboolean downcase)
{
	const guchar *cur;
	int i;
	
	g_return_val_if_fail (str, NULL);
	
	if (*str == '\0')
		return str;
	
	for (i = 0, cur = (const guchar*)str; *cur; ++cur) {

		if (G_LIKELY(*cur < 0xc3 || *cur > 0xc5)) {
			str[i++] = downcase ? tolower (*cur) : *cur;
			continue; 
		}
		
		if (*cur == 0xc3) { /* latin-1 supplement */
			++cur;
			switch (*cur) {
				
			case 0x80:
			case 0x81:
			case 0x82:
			case 0x83:
			case 0x84:
			case 0x85: str[i++] = downcase ? 'a' : 'A' ; break;

			case 0x86: 
				str[i++] = downcase ? 'a' : 'A' ;
				str[i++] = 'e';
				break;
				
			case 0x87: str[i++] = downcase ? 'c' : 'C'; break;
				
			case 0x88:
			case 0x89:
			case 0x8a:
			case 0x8b:
				str[i++] = downcase ? 'e' : 'E';
				break;
				
			case 0x8c:
			case 0x8d:
			case 0x8e:
			case 0x8f: str[i++] = downcase ? 'i': 'I'; break;
				
			case 0x90: str[i++] = downcase ? 'd' : 'D'; break;
			case 0x91: str[i++] = downcase ? 'n' : 'N'; break;
				
			case 0x92:
			case 0x93:
			case 0x94:
			case 0x95:
			case 0x96: str[i++] = downcase ? 'o' : 'O'; break;
				
			case 0x99:
			case 0x9a:
			case 0x9b:
			case 0x9c: str[i++] = downcase ? 'u' : 'U'; break;
			
			case 0x9d: str[i++] = downcase ? 'y' : 'Y'; break;

			case 0x9e:
				str[i++] = downcase ? 't' : 'T';
				str[i++] = 'h';
				break;
				
			case 0x9f: str[i++] = 's'; str[i++] = 's'; break;
				
			case 0xa0:
			case 0xa1:
			case 0xa2:
			case 0xa3:
			case 0xa4:
			case 0xa5: str[i++] = 'a'; break;
				
			case 0xa6: str[i++] = 'a'; str[i++] = 'e'; break;
			case 0xa7: str[i++] = 'c'; break;
				
			case 0xa8:
			case 0xa9:
			case 0xaa:
			case 0xab: str[i++] = 'e'; break;
				
			case 0xac:
			case 0xad:
			case 0xae:
			case 0xaf: str[i++] = 'i'; break;
				
			case 0xb0: str[i++] = 'd'; break;
			case 0xb1: str[i++] = 'n'; break;
				
			case 0xb2:
			case 0xb3:
			case 0xb4:
			case 0xb5:
			case 0xb6: str[i++] = 'o'; break;
				
			case 0xb9:
			case 0xba:
			case 0xbb:
			case 0xbc: str[i++] = 'u'; break;
				
			case 0xbd: str[i++] = 'y'; break;
			case 0xbe: str[i++] = 't'; str[i++] = 'h'; break;
			case 0xbf: str[i++] = 'y'; break;
				
			default:
				str[i++] = *cur;
			}

		} else if (*cur == 0xc4) {  /* Latin Extended-A (0x04) */
			++cur;
			switch (*cur) {	
			case 0x80:
			case 0x82:
			case 0x84: str[i++] = downcase ? 'a' : 'A'; break;

			case 0x86:
			case 0x88:
			case 0x8a:
			case 0x8c: str[i++] = downcase ? 'c' : 'C'; break;

			case 0x8e:
			case 0x90: str[i++] = downcase ? 'd' : 'D'; break;

			case 0x92:
			case 0x94:
			case 0x96:
			case 0x98:
			case 0x9a: str[i++] = downcase ? 'e' : 'E'; break;

			case 0x9c:
			case 0x9e:
			case 0xa0:
			case 0xa2: str[i++] = downcase ? 'g' : 'G'; break;

			case 0xa4:
			case 0xa6: str[i++] = downcase ? 'h' : 'H'; break;
				
			case 0xa8:
			case 0xaa:
			case 0xac:
			case 0xae:
			case 0xb0: str[i++] = downcase ? 'i' : 'I'; break;
				
			case 0xb2:
				str[i++] = downcase ? 'i' : 'I';
				str[i++] = downcase ? 'j' : 'J';
				break;
	

			case 0xb4: str[i++] = downcase ? 'j' : 'J'; break;

			case 0xb6: str[i++] = downcase ? 'k' : 'K'; break;

			case 0xb9:
			case 0xbb:
			case 0xbd:
			case 0xbf: str[i++] = downcase ? 'l': 'L'; break;
				
			case 0x81:
			case 0x83:
			case 0x85: str[i++] = 'a'; break;

			case 0x87:
			case 0x89:
			case 0x8b:
			case 0x8d: str[i++] = 'c'; break;

			case 0x8f:
			case 0x91: str[i++] = 'd'; break;

			case 0x93:
			case 0x95:
			case 0x97:
			case 0x99:
			case 0x9b: str[i++] = 'e'; break;

			case 0x9d:
			case 0x9f:
			case 0xa1:
			case 0xa: str[i++] = 'g'; break;

			case 0xa5:
			case 0xa7: str[i++] = 'h'; break;

			case 0xa9:
			case 0xab:
			case 0xad:
			case 0xaf:
			case 0xb1: str[i++] = 'i'; break;

			case 0xb3: str[i++] = 'i'; str[i++] = 'j'; break;

			case 0xb5: str[i++] = 'j'; break;

			case 0xb7:
			case 0xb8: str[i++] = 'k'; break;

			case 0xba:
			case 0xbc:
			case 0xbe: str[i++] = 'l'; break;

			default:   str[i++] = *cur; break;
				
			}

		} else { /* Latin Extended-A (0xc5) */
			++cur;
			switch (*cur) {
			case 0x81: str[i++] = downcase ? 'l': 'L'; break;
				
			case 0x83:
			case 0x85:
			case 0x87: str[i++] = downcase ? 'n': 'N'; break;

			case 0x8c:
			case 0x8e:
			case 0x90: str[i++] = downcase ? 'o': 'O'; break;
				
			case 0x92:
				str[i++] = downcase ? 'o':  'O';
				str[i++] = 'e';
				break;

			case 0x94:
			case 0x96:
			case 0x98: str[i++] = downcase ? 'r': 'R'; break;

			case 0x9a:
			case 0x9c:
			case 0x9e:
			case 0xa0: str[i++] = downcase ? 's': 'S'; break;

			case 0xa2:
			case 0xa4:
			case 0xa6: str[i++] = downcase ? 't': 'T'; break;

			case 0xa8:
			case 0xaa:
			case 0xac:
			case 0xae:
			case 0xb0:	
			case 0xb2: str[i++] = downcase ? 'u': 'U'; break;
			case 0xb4: str[i++] = downcase ? 'w': 'W'; break;

			case 0xb6:
			case 0xb8: str[i++] = downcase ? 'y': 'Y'; break;

			case 0xb9:
			case 0xbb: 
			case 0xbd: str[i++] = downcase ? 'z': 'Z'; break;
				
			case 0x80:
			case 0x82: str[i++] = 'l'; break;

			case 0x84:
			case 0x86:
			case 0x88:
			case 0x89:
			case 0x8a:
			case 0x8b: str[i++] = 'n'; break;

			case 0x8d:
			case 0x8f:
			case 0x91: str[i++] = 'o'; break;

			case 0x93: str[i++] = 'o'; str[i++] = 'e'; break;

			case 0x95:
			case 0x97:
			case 0x99: str[i++] = 'r'; break;

			case 0x9b:
			case 0x9d:
			case 0x9f:
			case 0xa1: str[i++] = 's'; break;

			case 0xa3:
			case 0xa5:
			case 0xa7: str[i++] = 't'; break;

			case 0xa9:
			case 0xab:
			case 0xad:
			case 0xaf:
			case 0xb1:	
			case 0xb3: str[i++] = 'u'; break;
				
			case 0xb5: str[i++] = 'w'; break;

			case 0xb7: str[i++] = 'y'; break;

			case 0xba:
			case 0xbc: 
			case 0xbe: str[i++] = 'z'; break;

			case 0xbf: str[i++] = 's'; break;
				
			default:   str[i++] = *cur; break;
			}
		}
	}

	str[i] = '\0';
	
	return str;
}
