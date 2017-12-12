-- ---------------------------------------------------------------------------
--                                                                          --
--                    SYSTEM M-1 STRING PROCESSING                          --
--                                                                          --
--                                 M-1                                      --
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

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction side" where things are not
--   finished yet or intended for to future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:


-- with ada.strings.unbounded; 	use ada.strings.unbounded;
with ada.directories;			use ada.directories;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings.maps;			use ada.strings.maps;
with ada.text_io;				use ada.text_io;
-- with ada.float_text_io;			use ada.float_text_io;

with ada.containers;            use ada.containers;
--with ada.containers.vectors;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_vectors;

-- with interfaces;				use interfaces;
-- with ada.exceptions;

with ada.calendar;				use ada.calendar;
with ada.calendar.formatting;	use ada.calendar.formatting;
with ada.calendar.time_zones;	use ada.calendar.time_zones;

with et_general;

package et_string_processing is

	type type_log_level is range 0..6;
	log_level : type_log_level := type_log_level'first;

	type type_indentation_level is range 0..20;
	log_indentation : type_indentation_level := type_indentation_level'first;
	procedure log_indentation_up;
	procedure log_indentation_down;
	procedure log_indentation_reset;

-- 	package indentation_level_stack is new 
-- 		et_general.stack_lifo (
-- 			max => natural(type_indentation_level'last),
-- 			item => type_indentation_level);
-- 	
-- 	type type_log_identation_operation is ( RESET, UP, DOWN, PUSH, POP);
-- 	procedure log_indentation_operation (operation : in type_log_identation_operation);
	
	function indent (width : in type_indentation_level) return string;
	
	procedure log (
		text	: in string;
		level	: in type_log_level := type_log_level'first;
		console	: in boolean := false);
	-- Writes the given text with the current log_indentation in the current output. 
	-- If the system wide log level is greater or equal the the given log_level the given text is put on the log.

-- CHARACTERS IN NOTES, TEXT, LABELS, ...
	-- Since we want designs readable and portable in as many languages as possible we accept only those characters:
	general_characters : character_set := 
		to_set (ranges => (('a','z'),('A','Z'),('0','9'))) -- letters and numbers
		or to_set ("!?.-: "); -- other characters -- CS: add others if neccessary
	
-- DATE
	date_characters : character_set := to_set (span => ('0','9')) or to_set ("-:T");
	type type_date is new string (1..19); -- "2017-08-17T14:17:25" -- CS: define a type that allows only those characters

	function to_string (date : in type_date) return string;
	-- Returns the given date as string.

	function date_valid (date : in type_date) return boolean;
	-- Returns true if given date is valid and plausible.
	
	function date_now return type_date;

	procedure check_updated_vs_commissioned (commissioned , updated : in type_date);
	-- Checks whether updated is later or equal commissioned.

	row_separator_length 	: constant positive := 150;
	row_separator_single	: constant string (1..row_separator_length)	:= row_separator_length * "-";	
	row_separator_double	: constant string (1..row_separator_length)	:= row_separator_length * "=";
	item_not_specified		: constant string (1..7) := "missing";
	
-- WARNING AND ERROR MESSAGES
	message_error : constant string (1..8)	:= "ERROR ! ";
	type type_warning_counter is new natural; -- CS: rename to type_warnings_counter. should be private
	warning_counter : type_warning_counter := 0;
	procedure reset_warnings_counter;
	function message_warning return string;
	-- Returns a warning string and increments the warning counter.

	function message_note return string;
	-- Returns a notification string.

	mounted		: constant string (1..7)  := "mounted";
	not_mounted	: constant string (1..11) := "not mounted";
	
-- TIMESTAMP
	timestamp_characters : character_set := to_set (ranges => (('A','F'),('0','9'))); -- CS: upper case letters only	
	type type_timestamp is new string (1..8); -- like "3459A3C1"
	procedure check_timestamp (timestamp : in type_timestamp);
	-- Checks the given timestamp for valid characters and plausible time.
	

	function strip_directory_separator (text : in string) return string;
	-- Removes the trailing directory separtor (if preset).
	
	short_string_length_max : constant natural := 10;
 	package type_short_string is new generic_bounded_length(short_string_length_max);

	universal_string_length_max	: constant natural := 1000;
	package type_universal_string is new generic_bounded_length(universal_string_length_max);
	
	long_string_length_max	: constant natural := 10000;
	package type_long_string is new generic_bounded_length(long_string_length_max);
	
	extended_string_length_max	: constant natural := 100000;
	package type_extended_string is new generic_bounded_length(extended_string_length_max);

	function ht_to_space (c : in character) return character;

	function tilde_to_space (c : in character) return character;
	-- Replaces a tilde by space. Other characters are returned unchanged.
	
	function wildcard_match (text_with_wildcards : in string; text_exact : in string) return boolean;
	-- Returns true if text_with_wildcards matches text_exact.
	-- text_with_wildcards is something like R41* , text_exact is something like R415
	
	function remove_comment_from_line(
		text_in : in string;					-- the input string
		comment_mark : in string;				-- the comment mark (like "--" or "#"
		test_whole_line : in boolean := true	-- when false, cares for the comment mark at line begin only
		)										-- further comment marks are ignored
		return string;

	function get_field_count (text_in : string) return natural;

	function strip_quotes (text_in : in string) return string;
	-- removes heading and trailing quotation from given string

	function enclose_in_quotes (text_in : in string; quote : in character := latin_1.apostrophe) return string;
	-- Adds heading and trailing quotate to given string. NOTE: apostrophe is ', quotation is "

	function trim_space_in_string (text_in : in string) return string;
	-- shrinks successive space characters to a single one in given string


	procedure write_message (
		file_handle : in ada.text_io.file_type;
		identation : in natural := 0;
		text : in string;
		lf   : in boolean := true;
		file : in boolean := true;
		console : in boolean := false);
	
	function get_field_from_line (
	-- Extracts a field separated by ifs at position. If trailer is true, the trailing content untiil trailer_to is also returned.
		text_in 	: in string;
		position 	: in positive;
		ifs 		: in character := latin_1.space;
		trailer 	: in boolean := false;
		trailer_to 	: in character := latin_1.semicolon
		) return string;

	-- CS: comments
	package type_list_of_strings is new indefinite_vectors (index_type => positive, element_type => string);

	-- This type is required when reading lines from files. It is a composite type
	-- whose components are hidden. The can only be accessed by special functions and procedures. See below.
	type type_fields_of_line is private;

	function read_line(
	-- Breaks down a given string and returns a type_fields_of_line.
		line			: in string; -- the line to be broken down
		number			: in positive_count := positive_count'first; -- the line number	
		comment_mark	: in string := et_general.comment_mark; -- the comment mark like "--" or "#"
		test_whole_line	: in boolean := true; -- when false, cares for the comment mark at line begin only
												 -- further comment marks are ignored
		ifs				: in character := latin_1.space;	-- field separator
		delimiter_wrap	: in boolean := false; -- true if text in delimiters is to be wrapped into a single field
		delimiter		: in character := latin_1.quotation -- the text delimiter sign (mostly ")
		) return type_fields_of_line;
	
	function append (left : in type_fields_of_line; right : in type_fields_of_line) return type_fields_of_line;
	
	function get_field_from_line (line : in type_fields_of_line; position : in positive) return string; 
	-- CS: rename to "field". position should be a count_type

	function to_string ( line : in type_fields_of_line) return string;

	function affected_line ( line : in type_fields_of_line ) return string;
	-- Returns the line number of the given line in a string like "line x:"

	function field_count ( line : in type_fields_of_line) return count_type;
	-- Returns the number of fields in the given line.

	function lines_equally (left, right : in type_fields_of_line) return boolean;
	
	private
		type type_fields_of_line is record
			fields		: type_list_of_strings.vector;
			field_count	: count_type;		-- number of fields in line
			number		: positive_count; 	-- line numer
		end record;


		
end et_string_processing;

-- Soli Deo Gloria
