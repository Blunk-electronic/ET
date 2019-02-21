------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET KICAD                                --
--                                                                          --
--                                 ET                                       --
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
--   ToDo:
--		1. Warning if virtual component pins apply for all units. Usually 
--			virtual components (such as power flags) have only one unit. If the
--			check "common to all units in component" is set, ET generates an
--			extra unit. Why ? ET assumes the affeced pin is a power pin. Power pins
--			in turn are assigned to an extra unit (in EAGLE we speak of "supply symbols").
--		2. Warning if virtual component with one power pin has pin direction differing from power_out
--			Example: Power symbol "P3V3" must have pin direction power_out.	
--		3. Make sure ports of netchangers are named like 1 or 2.

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.text_io;				use ada.text_io;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.directories;			use ada.directories;
with ada.exceptions; 			use ada.exceptions;
with ada.environment_variables;

with et_coordinates;
with et_libraries;
with et_schematic;
with et_geometry;

with et_general;
with et_string_processing;		use et_string_processing;
with et_project;
with et_pcb;
with et_pcb_coordinates;
with et_kicad_pcb;
with et_export;
with et_csv;

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


	
end et_kicad_general;

-- Soli Deo Gloria
