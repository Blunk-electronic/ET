------------------------------------------------------------------------------
--                                                                          --
--                    SYSTEM ET GENERAL DECLARATIONS                        --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 Mario Blunk, Blunk electronic                 --
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

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.containers; 			use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.vectors;
with ada.containers.ordered_maps;
with ada.containers.ordered_sets;

with ada.text_io;				use ada.text_io;
--with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.strings.bounded;       use ada.strings.bounded;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.calendar;				use ada.calendar;
with ada.calendar.time_zones;	use ada.calendar.time_zones;
with ada.calendar.formatting;	use ada.calendar.formatting;

package et_general is

	system_name						: constant string (1..9) := "SYSTEM ET";

	-- FILES, EXTENSIONS AND DIRECTORY NAMES

	-- The name of a project may have 100 characters which seems sufficient for now.
 	project_name_length		: constant natural := 100;

	-- All reports go into this directory:
	report_directory		: constant string (1..10)	:= "et_reports";
	report_extension		: constant string (1..3)	:= "log";
	
    -- FILE HANDLES

	
	-- FREQUENTLY USED WORDS AND PHRASES
	message_error					: constant string (1..8) := "ERROR ! ";
	message_warning					: constant string (1..10) := "WARNING ! ";	
	row_separator_single			: constant string (1..100) := 100 * "-";	
	row_separator_double			: constant string (1..100) := 100 * "=";
	
	-- COMMAND LINE SWITCHES
	--switch_about			: constant string (1..7) := "--about"; -- CS
	switch_version			: constant string (1..8)	:= "-version"; -- long switch
	switch_help				: constant string (1..5)	:= "-help"; -- long switch	
--	switch_import_file		: constant string (1..12)	:= "-import_file";	-- long switch -- currently we do not care about importing single files
	switch_import_project	: constant string (1..15)	:= "-import_project";	-- long switch
	switch_import_format	: constant string (1..14)	:= "-import_format";	-- long switch
																															
-- 	-- ACTIONS
-- 	type type_action is ( none, request_version, import_cad );

    
	-- The name of the person who has drawn, checked or approved something may have 100 characters which seems sufficient for now.
 	person_name_length	: constant natural := 100;
	package type_person_name is new generic_bounded_length(person_name_length); use type_person_name;


	-- LIBRARY NAMES AND DIRECTORIES

	-- For storing bare library names like "bel_primitives" we use this bounded string:
	library_name_length_max : constant natural := 100; -- CS: increase if necessary
    package type_library_name is new generic_bounded_length(library_name_length_max); use type_library_name;

	-- Bare library names can be stored further-on in an ordered set like this:
	-- We use a doubly linked list because the order of the library names sometimes matters.
    package type_list_of_library_names is new doubly_linked_lists (
		element_type => type_library_name.bounded_string);

	-- The base directory where libraries live is stored in a bounded string:
	library_directory_length_max : constant positive := 300; -- CS: increase if necessary
	package type_library_directory is new generic_bounded_length(library_directory_length_max); use type_library_directory;

	-- If a library is fully specified with path, name and extension we store them in bounded strings:
	library_full_name_max : constant positive := library_directory_length_max + library_name_length_max + 4;
	package type_library_full_name is new generic_bounded_length(library_full_name_max); use type_library_full_name;

	-- Full library names can be stored furhter-on in an ordered set like this:
	-- We use a doubly linked list because the order of the library names sometimes matters.
	package type_list_of_full_library_names is new doubly_linked_lists (
		element_type => type_library_full_name.bounded_string);


	
    -- PAPER SIZES
    type type_paper_size is ( A0, A1, A2, A4 ); -- CS: others
    paper_size_default : type_paper_size := A4;
    


	
	-- GENERICS
	
	generic
		max : positive;
		type item is private;
	package stack_lifo is
		procedure push (x : item);
		function pop return item;
		function depth return natural;
		procedure init;
	end stack_lifo;
	
	
end et_general;

