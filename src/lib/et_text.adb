------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                                TEXT                                      --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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

with et_string_processing;

package body et_text is

	package body text is
		use et_string_processing;

		function to_string (size : in type_distance) return string is begin
			return type_distance'image (size);
		end;

-- 		function to_string (rotation : in type_rotation) return string is begin
-- 			return type_rotation'image (rotation);
-- 		end;

		
		procedure validate_text_size (size : in type_distance) is
		-- Checks whether given text size is in range of type_text_size.
		begin
			if size not in type_text_size then
				log (ERROR, "text size invalid ! Allowed range is" 
					& to_string (type_text_size'first) & " .."
					& to_string (type_text_size'last),
					console => true);
				raise constraint_error;
			end if;
		end validate_text_size;

		procedure validate_text_line_width (width : in type_distance) is
		-- Checks whether given line width is in range of type_text_line_width
		begin
			if width not in type_text_line_width then
				log (ERROR, "line width invalid ! Allowed range is" 
					& to_string (type_text_line_width'first) & " .."
					& to_string (type_text_line_width'last),
					console => true);
				raise constraint_error;
			end if;
		end validate_text_line_width;


		function text_properties (text : in type_text) return string is
		-- Returns the properties of the given text in a long single string.
		begin
			return --to_string (text.position) & latin_1.space
				"size (width/height)" 
				& to_string (text.dimensions.width) & " / " & to_string (text.dimensions.height)
				& " line width" & to_string (text.line_width)
-- 				& " rotation" & to_string (rot (text.position))
-- CS				& et_libraries.to_string (text.alignment)
				;
		end text_properties;
		
	end text;

end et_text;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
