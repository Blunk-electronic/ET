------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       BOARD SHAPES AND TEXT                              --
--                                                                          --
--                              B o d y                                     --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
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

with ada.characters.handling;	use ada.characters.handling;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;

package body et_board_shapes_and_text is
	
	procedure validate_general_line_width (
		width : in et_pcb_coordinates_2.type_distance_model) 
	is begin
		if width not in type_general_line_width then
			log (ERROR, "line width invalid ! Allowed range is" 
				 & to_string (type_general_line_width'first) & " .."
				 & to_string (type_general_line_width'last),
				 console => true);
			raise constraint_error;
		end if;
	end validate_general_line_width;


	


	function to_layer_category (cat : in string) return type_layer_category is begin
		return type_layer_category'value (layer_category_prefix & cat);
	end to_layer_category;

	function to_string (cat : in type_layer_category) return string is
		s : string := type_layer_category'image (cat);
	begin
		return s (layer_category_prefix'length + 1 .. s'last);
	end to_string;

	

	function face_to_mirror (f : in type_face) 
		return et_text.type_vector_text_mirrored 
	is 
		use et_text;
	begin
		case f is
			when TOP	=> return NO;
			when BOTTOM	=> return YES;
		end case;
	end face_to_mirror;

	
end et_board_shapes_and_text;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
