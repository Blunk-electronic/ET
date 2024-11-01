------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            KICAD GENERAL                                 --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                -- 
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

--   The two letters "CS" indicate a "construction side" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.text_io;				use ada.text_io;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;
with ada.containers.doubly_linked_lists;

with et_package_names;			use et_package_names;
with et_devices;

with et_string_processing;		use et_string_processing;

package et_kicad_general is

	system_name	: constant string := "KiCad";

	comment_mark : constant string := "#";

	
-- V4
	pcb_new_version_4_0_7		: constant string := "4.0.7";
	pcb_file_format_version_4	: constant string := "4";
	
	host_name_pcbnew			: constant string := "pcbnew";

-- V5
	pcb_new_version_5_0_0		: constant string := "5.0.0-5.0.0"; -- CS update version number or find better solution
	-- Newly created projects without a board have a line like:
	--  (kicad_pcb (version 4) (host kicad "dummy file") )
	-- For this reason we need a constant:
	host_name_pcbnew_dummy_v5	: constant string := "kicad";


    encoding_default 					: constant string := "utf-8";	

	file_extension_project   			: constant string := "pro";
	file_extension_schematic 			: constant string := "sch";
	file_extension_schematic_lib		: constant string := "lib";
	file_extension_board	 			: constant string := "kicad_pcb";

	schematic_version_v4	: constant positive := 2; -- CS use dedicated type for schematic version
    schematic_version_v5	: constant positive := 4;

	
	
	-- If lines of a file are to be collected we use this simple list:
	package pac_lines_of_file is new doubly_linked_lists (
		element_type	=> type_fields_of_line,
		"=" 			=> lines_equally);


	
-- TIMESTAMP
	timestamp_characters : character_set := to_set (ranges => (('A','F'),('0','9'))); -- CS: upper case letters only	
	type type_timestamp is new string (1..8); -- like "3459A3C1"
	timestamp_default : constant type_timestamp := "00000000";
	
	procedure check_timestamp (timestamp : in type_timestamp);
	-- Checks the given timestamp for valid characters and plausible time.

	
-- LIBRARIES
	-- To handle library paths we (mis)use pac_device_model_file 
	-- and pac_package_model_file_name under different names:
	-- package type_package_library_name renames pac_package_model_file_name;
	package type_device_library_name renames et_devices.pac_device_model_file;

	library_name_length_max : constant natural := 100; -- CS: increase if necessary
	
	-- For storing BARE library names like "bel_primitives" we use this bounded string:
	package type_library_name is new generic_bounded_length (library_name_length_max); 
	use type_library_name;

	function to_library_name (library_name : in string) return type_library_name.bounded_string;
	-- converts a string to a type_library_name
	
	-- CS: for type_library_name: character set, check characters, check length
	
	function to_string (library_name : in type_library_name.bounded_string) return string;
	-- Returns the given library name as string.

	

	-- Libraries are stored in directories:
	library_directory_length_max : constant positive := 300; -- CS: increase if necessary
	package type_library_directory is new generic_bounded_length (library_directory_length_max);

	function to_string (dir : in type_library_directory.bounded_string) return string;

	-- Search list for library directories.
	-- This list applies for both component and package search operations.
	package type_project_lib_dirs is new doubly_linked_lists (
		element_type	=> type_library_directory.bounded_string,
		"=" 			=> type_library_directory."=");
	search_list_project_lib_dirs : type_project_lib_dirs.list;


	
end et_kicad_general;

-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
