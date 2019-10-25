------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                        COMPONENT LIBRARIES                               --
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

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.text_io;				use ada.text_io;
with ada.strings; 				use ada.strings;
--with ada.strings.maps;
with ada.strings.maps.constants;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.strings.unbounded; 	use ada.strings.unbounded;
with ada.numerics.real_arrays;  use ada.numerics.real_arrays;
with ada.directories;			use ada.directories;
with ada.exceptions; 			use ada.exceptions;

with et_string_processing;
with et_coordinates;
with et_import;
with et_text;

with et_devices;				use et_devices;

package body et_libraries is


	
	
	function to_string (person : in type_person_name.bounded_string) return string is
	-- Returns the given person name as string.
	begin
		return latin_1.space & type_person_name.to_string (person);
	end to_string;

-- 	function to_string ( -- CS remove
-- 		size		: in pac_text.type_text_size;
-- 		preamble	: in boolean := true) return string is
-- 	-- Returns the given text size as string.
-- 	begin
-- 		if preamble then
-- 			return "size " & geometry.to_string (size);
-- 		else
-- 			return geometry.to_string (size);
-- 		end if;
-- 	end to_string;

	

	

	


	function to_string (name : in type_frame_template_name.bounded_string) return string is begin
		return type_frame_template_name.to_string (name);
	end to_string;
	
	function to_template_name (name : in string) return type_frame_template_name.bounded_string is begin
		return type_frame_template_name.to_bounded_string (name);
	end to_template_name;
	


	
	
end et_libraries;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
