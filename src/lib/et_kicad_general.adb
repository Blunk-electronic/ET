------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            KICAD GENERAL                                 --
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

--   The two letters "CS" indicate a "construction side" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

package body et_kicad_general is

	
	procedure check_timestamp (timestamp : in type_timestamp) is
	-- Checks the given timestamp for valid characters and plausible time.
	begin
		null; -- CS
	end check_timestamp;
	
	function to_library_name (library_name : in string) return type_library_name.bounded_string is
	-- converts a string to a type_library_name
	begin
		return type_library_name.to_bounded_string (library_name);
	end to_library_name;
	
	function to_string (library_name : in type_library_name.bounded_string) return string is
	-- Returns the given library name as string.
	begin
		return type_library_name.to_string (library_name);
	end to_string;

	function to_string (dir : in type_library_directory.bounded_string) return string is
	begin
		return type_library_directory.to_string (dir);
	end to_string;

	
end et_kicad_general;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
