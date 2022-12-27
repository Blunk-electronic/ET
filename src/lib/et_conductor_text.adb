------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                           CONDUCTOR TEXT                                 --
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

with ada.text_io;				use ada.text_io;



package body et_conductor_text is


	function face_to_mirror (
		f : in type_face)
		return type_vector_text_mirrored 
	is begin
		case f is
			when TOP	=> return NO;
			when BOTTOM	=> return YES;
		end case;
	end face_to_mirror;



	procedure mirror_text (
		text	: in out type_conductor_text;
		axis	: in type_axis_2d := Y)
	is begin
		mirror_vector_text (text.vectors);
	end mirror_text;


	procedure rotate_text (
		text	: in out type_conductor_text;
		angle	: in type_rotation)
	is begin
		rotate_vector_text (text.vectors, angle);
	end rotate_text;


	procedure move_text (
		text	: in out type_conductor_text;
		offset	: in type_distance_relative)
	is begin
		move_vector_text (text.vectors, offset);
	end move_text;

	

	function to_polygons (
		text		: in type_conductor_text;
		tolerance	: in type_distance_positive;
		debug		: in boolean := false)					 
		return pac_polygon_list.list
	is
		result : pac_polygon_list.list;

		linewidth : constant type_float_positive := 
			type_float_positive (get_linewidth (text.vectors));

		tolerance_float : constant type_float_positive := 
			type_float_positive (tolerance);


		use pac_polygon_union;
		
		procedure query_line (l : in pac_character_lines.cursor) is
			use pac_character_lines;
			p : type_polygon := to_polygon (
				type_line_fine (element (l)), linewidth, tolerance_float, EXPAND);
		begin
			if debug then
				put_line (to_string (element (l)));
				put_line (to_string (p));
			end if;
			
			--put_line ("line");
			result.append (p);
			-- don't ! multi_union (result);
		end query_line;
		
	begin
		iterate (text.vectors, query_line'access);
		return result;
	end to_polygons;
	

	
end et_conductor_text;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
