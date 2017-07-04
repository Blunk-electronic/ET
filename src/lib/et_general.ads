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
--   Mario.Blunk@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

-- with ada.strings.bounded; 	use ada.strings.bounded;
-- with ada.containers; use ada.containers;
-- with ada.containers.doubly_linked_lists;
-- with ada.containers.vectors;
-- 

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

    -- FILE HANDLES
	handle_project		: ada.text_io.file_type;
	handle_schematic	: ada.text_io.file_type;
	handle_report		: ada.text_io.file_type;
	--file_board		: ada.text_io.file_type; CS
	
	-- FREQUENTLY USED WORDS AND PHRASES
	message_error					: constant string (1..8) := "ERROR ! ";
	message_warning					: constant string (1..10) := "WARNING ! ";	
	row_separator_single			: constant string (1..100) := 100 * "-";	
	row_separator_double			: constant string (1..100) := 100 * "=";
	
	-- ARGUMENT KEYWORDS
	argument_keyword_about 			: constant string (1..7) := "--about"; -- CS
	argument_keyword_version 		: constant string (1..9) := "--version";
	argument_keyword_import 		: constant string (1..8) := "--import";
																															
	-- ACTIONS
	type type_action is ( none, request_version, import_cad );

	-- The name of a project may have 100 characters which seems sufficient for now.
 	project_name_length	: constant natural := 100;
	package type_project_name is new generic_bounded_length(project_name_length); use type_project_name;
    
	-- The name of the person who has drawn, checked or approved something may have 100 characters which seems sufficient for now.
 	person_name_length	: constant natural := 100;
	package type_person_name is new generic_bounded_length(person_name_length); use type_person_name;

    -- PAPER SIZES
    type type_paper_size is ( A0, A1, A2, A4 ); -- CS: others
    paper_size_default : type_paper_size := A4;
    
    -- STRING PROCESSING
    encoding_default : constant string (1..5) := "utf-8";
    
	function get_field
	-- Extracts a field separated by ifs at position. If trailer is true, the trailing content untiil trailer_to is also returned.
			(
			text_in 	: in string;
			position 	: in positive;
			ifs 		: in character := latin_1.space;
			trailer 	: boolean := false;
			trailer_to 	: in character := latin_1.semicolon
			) return string;

	function strip_quotes (text_in : in string) return string;
	-- removes heading and trailing quotation from given string		

	-- MESSAGES
	procedure write_message (
		file_handle : in ada.text_io.file_type;
		identation : in natural := 0;
		text : in string;
		lf   : in boolean := true;
		file : in boolean := true;
		console : in boolean := false);


	
    -- TIME OPERATIONS
    -- CS: specifiy a type_date for something like 2017-02-12T18:45:01
    date_field_length   : constant positive := 19;
	now				    : time := clock;
	date_now            : string (1..date_field_length) := image(now, time_zone => UTC_Time_Offset(now));

	
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

