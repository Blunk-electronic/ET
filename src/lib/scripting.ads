------------------------------------------------------------------------------
--                                                                          --
--                          SYSTEM ET SCRIPTING                             --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               S p e c                                    --
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

--   For correct displaying set tab width in your editor to 4.

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

with ada.text_io;				use ada.text_io;
with ada.characters.latin_1;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded;       use ada.strings.bounded;
with ada.containers;            use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with et_general;
with et_coordinates;			use et_coordinates;
with et_libraries;
with et_string_processing;		use et_string_processing;
with et_schematic;
with et_pcb;

package scripting is
	comment_mark : constant string := ("--");

	script_name_length_max : constant positive := 100; -- CS increase if necessary
	package type_script_name is new generic_bounded_length (script_name_length_max);
	
	function to_string (name : in type_script_name.bounded_string) return string;
	function to_script_name (name : in string) return type_script_name.bounded_string;
	
	script_name : type_script_name.bounded_string;
									  

	type type_exit_code is (
		SUCCESSFUL,
		WARNINGS,
		ERROR
		);

	function execute_script (
		file_name		: in type_script_name.bounded_string;
		log_threshold	: in type_log_level)
		return type_exit_code;

end scripting;

-- Soli Deo Gloria
