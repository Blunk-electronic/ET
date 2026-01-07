------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         SILKSCREEN PACKAGES                              --
--                                                                          --
--                              B o d y                                     --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
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

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   to do:

with ada.strings;				use ada.strings;

package body et_silkscreen.packages is

	procedure mirror_silkscreen_objects (
		silkscreen	: in out type_silkscreen_package;
		axis		: in type_mirror := MIRROR_ALONG_Y_AXIS)
	is begin
		mirror_lines (silkscreen.lines, axis);
		mirror_arcs (silkscreen.arcs, axis);
		mirror_circles (silkscreen.circles, axis);
		mirror_texts (silkscreen.texts, axis);
		mirror_contours (silkscreen.zones, axis);
		mirror_placeholders (silkscreen.placeholders, axis);
	end mirror_silkscreen_objects;


	procedure rotate_silkscreen_objects (
		silkscreen	: in out type_silkscreen_package;
		angle		: in type_rotation_model)
	is begin
		rotate_lines (silkscreen.lines, angle);
		rotate_arcs (silkscreen.arcs, angle);
		rotate_circles (silkscreen.circles, angle);
		rotate_texts (silkscreen.texts, angle);
		rotate_contours (silkscreen.zones, angle);
		rotate_placeholders (silkscreen.placeholders, angle);
	end rotate_silkscreen_objects;



	procedure move_silkscreen_objects (
		silkscreen	: in out type_silkscreen_package;
		offset		: in type_vector_model)
	is begin
		move_lines (silkscreen.lines, offset);
		move_arcs (silkscreen.arcs, offset);
		move_circles (silkscreen.circles, offset);
		move_texts (silkscreen.texts, offset);
		move_contours (silkscreen.zones, offset);
		move_placeholders (silkscreen.placeholders, offset);
	end move_silkscreen_objects;





	procedure add_zone (
		silkscreen	: in out type_silkscreen_both_sides;
		zone		: in type_silk_zone;
		face		: in type_face)
	is 
		use pac_silk_zones;
	begin
		case face is
			when TOP => 
				append (silkscreen.top.zones, zone);


			when BOTTOM => 
				append (silkscreen.bottom.zones, zone);
		end case;
	end;

	


	

	procedure add_text (
		silkscreen	: in out type_silkscreen_both_sides;
		text		: in type_silk_text;
		face		: in type_face)
	is 
		use pac_silk_texts;
	begin
		case face is
			when TOP => 
				append (silkscreen.top.texts, text);


			when BOTTOM => 
				append (silkscreen.bottom.texts, text);
		end case;
	end;


	


	procedure add_placeholder (
		silkscreen	: in out type_silkscreen_both_sides;
		placeholder	: in type_text_placeholder;
		face		: in type_face)
	is
		use pac_text_placeholders;
	begin
		case face is
			when TOP => 
				append (silkscreen.top.placeholders, placeholder);


			when BOTTOM => 
				append (silkscreen.bottom.placeholders, placeholder);
		end case;
	end;

	
	
	
end et_silkscreen.packages;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
