------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       BOARD VERB AND NOUN KEYS                           --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                --
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


	key_verb_draw		: constant gdk_key_type := GDK_LC_d;
	key_verb_move		: constant gdk_key_type := GDK_LC_m;
	key_verb_rotate		: constant gdk_key_type := GDK_LC_o;
	key_verb_place		: constant gdk_key_type := GDK_LC_p;
	-- CS add others
	

	-- CS ? key_noun_contour		: constant gdk_key_type := GDK_LC_c;
	key_noun_text		: constant gdk_key_type := GDK_LC_x;
	key_noun_zone		: constant gdk_key_type := GDK_LC_z;
	-- CS add others
	
end et_board_verb_noun_keys;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
