------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       BOARD VERB AND NOUN KEYS                           --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   ToDo: 


with gdk.types;						use gdk.types;
with gdk.types.keysyms;				use gdk.types.keysyms;


package et_board_verb_noun_keys is

-- VERBS:
	
	key_verb_clear		: constant gdk_key_type := GDK_LC_c;
	key_verb_delete		: constant gdk_key_type := GDK_Delete;
	key_verb_draw		: constant gdk_key_type := GDK_LC_d;
	key_verb_fill		: constant gdk_key_type := GDK_LC_f;
	key_verb_flip		: constant gdk_key_type := GDK_LC_l;
	key_verb_move		: constant gdk_key_type := GDK_LC_m;
	key_verb_rename		: constant gdk_key_type := GDK_LC_n;
	key_verb_rotate		: constant gdk_key_type := GDK_LC_o;
	key_verb_route		: constant gdk_key_type := GDK_LC_r;
	key_verb_place		: constant gdk_key_type := GDK_LC_p;
	key_verb_show		: constant gdk_key_type := GDK_LC_h;
	key_verb_update		: constant gdk_key_type := GDK_LC_u;



	
-- NOUNS:	

	key_noun_assy		: constant gdk_key_type := GDK_LC_a;
	key_noun_device		: constant gdk_key_type := GDK_LC_d;
	key_noun_freetrack	: constant gdk_key_type := GDK_LC_f;
	key_noun_line		: constant gdk_key_type := GDK_LC_l;
	key_noun_outline	: constant gdk_key_type := GDK_LC_o;
	
	-- CS: This is a multi-assignment to key "n".
	-- For the moment it seems not to cause trouble:
	key_noun_net 		: constant gdk_key_type := GDK_LC_n;
	
	-- CS ? key_noun_contour		: constant gdk_key_type := GDK_LC_c;

	key_noun_ratsnest	: constant gdk_key_type := GDK_LC_r;
	key_noun_silkscreen	: constant gdk_key_type := GDK_LC_s;
	key_noun_stopmask	: constant gdk_key_type := GDK_LC_t;
	key_noun_stencil	: constant gdk_key_type := GDK_LC_e;
	key_noun_keepout	: constant gdk_key_type := GDK_LC_k;
	key_noun_conductors	: constant gdk_key_type := GDK_LC_c;
	key_noun_text		: constant gdk_key_type := GDK_LC_x;
	-- key_noun_track		: constant gdk_key_type := GDK_LC_t;
	key_noun_via		: constant gdk_key_type := GDK_LC_v;
	key_noun_zone		: constant gdk_key_type := GDK_LC_z;



	
-- MISCELLANEOUS:
	
	key_space			: constant gdk_key_type := GDK_Space;
	key_mode			: constant gdk_key_type := GDK_LC_m;
	key_bend_style		: constant gdk_key_type := GDK_LC_b;
	key_clarify			: constant gdk_key_type := GDK_page_down;

end et_board_verb_noun_keys;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
