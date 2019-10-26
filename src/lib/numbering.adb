------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             NUMBERING                                    --
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

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction side" where things are not
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
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.text_io;				use ada.text_io;

with ada.exceptions;
with ada.directories;
with gnat.directory_operations;

with ada.containers;            use ada.containers;

with et_general;				use et_general;
-- with et_string_processing;		use et_string_processing;
with et_devices;				use et_devices;

package body numbering is

	procedure dummy is begin null; end;

	function "<" (left, right : in type_module) return boolean is
		use type_module_name;
		use type_module_instance_name;
		result : boolean;
	begin
		if left.name < right.name then
			result := true;
		elsif left.name > right.name then
			result := false;
		else -- names equal
			if left.instance < right.instance then
				result := true;
			elsif left.instance > right.instance then
				result := false;
			else -- instances equal
				result := false;
			end if;
		end if;

		return result;
	end "<";
			
	function to_index_range (
	-- Returns a string like "module 'templates/clock_generator' range 78 .. 133"
		module_name	: in type_module_name.bounded_string;
		index_range	: in type_index_range) return string is
	begin
		return ("module " & enclose_in_quotes (to_string (module_name)) &
			" range" & to_string (index_range.lowest) &
			" .." & to_string (index_range.highest));
	end to_index_range;

	function below (left, right : in type_index_range) return boolean is begin
	-- Returns true if left index range is below right index range.
		if left.highest < right.lowest then
			return true;
		else
			return false;
		end if;
	end;
		
	function above (left, right : in type_index_range) return boolean is begin
	-- Returns true if left index range is above right index range.		
		if left.lowest > right.highest then
			return true;
		else
			return false;
		end if;
	end;

	
end numbering;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
