------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                  ASSEMBLY DOCUMENTAION PACKAGES                          --
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
with et_text;					use et_text;

package body et_assy_doc.packages is

	procedure mirror_assy_doc_objects (
		assy_doc	: in out type_assy_doc_package;
		axis		: in type_mirror := MIRROR_ALONG_Y_AXIS)
	is begin
		mirror_lines (assy_doc.lines, axis);
		mirror_arcs (assy_doc.arcs, axis);
		mirror_circles (assy_doc.circles, axis);
		mirror_texts (assy_doc.texts, axis);
		mirror_zones (assy_doc.zones, axis);
		mirror_placeholders (assy_doc.placeholders, axis);
	end mirror_assy_doc_objects;


	procedure rotate_assy_doc_objects (
		assy_doc	: in out type_assy_doc_package;
		angle		: in type_rotation_model)
	is begin
		rotate_lines (assy_doc.lines, angle);
		rotate_arcs (assy_doc.arcs, angle);
		rotate_circles (assy_doc.circles, angle);
		rotate_texts (assy_doc.texts, angle);
		rotate_zones (assy_doc.zones, angle);
		rotate_placeholders (assy_doc.placeholders, angle);
	end rotate_assy_doc_objects;



	procedure move_assy_doc_objects (
		assy_doc	: in out type_assy_doc_package;
		offset		: in type_vector_model)
	is begin
		move_lines (assy_doc.lines, offset);
		move_arcs (assy_doc.arcs, offset);
		move_circles (assy_doc.circles, offset);
		move_texts (assy_doc.texts, offset);
		move_zones (assy_doc.zones, offset);
		move_placeholders (assy_doc.placeholders, offset);
	end move_assy_doc_objects;




	procedure add_zone (
		assy_doc	: in out type_assy_doc_both_sides;
		zone		: in type_doc_zone;
		face		: in type_face)
	is 
		use pac_doc_zones;
	begin
		case face is
			when TOP => 
				append (assy_doc.top.zones, zone);


			when BOTTOM => 
				append (assy_doc.bottom.zones, zone);
		end case;
	end;

	



	procedure add_text (
		assy_doc	: in out type_assy_doc_both_sides;
		text		: in type_doc_text;
		face		: in type_face)
	is 
		use pac_doc_texts;
	begin
		case face is
			when TOP => 
				append (assy_doc.top.texts, text);


			when BOTTOM => 
				append (assy_doc.bottom.texts, text);
		end case;
	end;
	


	

	procedure add_placeholder (
		assy_doc	: in out type_assy_doc_both_sides;
		placeholder	: in type_text_placeholder;
		face		: in type_face)
	is
		use pac_text_placeholders;
	begin
		case face is
			when TOP => 
				append (assy_doc.top.placeholders, placeholder);


			when BOTTOM => 
				append (assy_doc.bottom.placeholders, placeholder);
		end case;
	end;

	
end et_assy_doc.packages;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
