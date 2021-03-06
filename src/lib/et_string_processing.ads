-- ---------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           STRING PROCESSING                              --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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

with ada.directories;			use ada.directories;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings.maps;			use ada.strings.maps;
with ada.text_io;				use ada.text_io;
with ada.containers;            use ada.containers;
with ada.containers.indefinite_vectors;

with ada.calendar;				use ada.calendar;
with ada.calendar.formatting;	use ada.calendar.formatting;
with ada.calendar.time_zones;	use ada.calendar.time_zones;

with et_general;

package et_string_processing is

	-- The log level is limited to a reasonable value.
	log_level_max : constant positive := 50; -- CS increase if neccessary
	-- Functions and procedures pass each other this log level type.
	-- Theoretically it may assume indefinite values (for example during creation of 
	-- routing tables). However, it is limited to a reasonable value. See above.
	type type_log_level is range 0..log_level_max;

-- 	no_logging : constant type_log_level := type_log_level'last;
	
	function to_string (
	-- Returns the given log level as string. 
		log_level	: in type_log_level;
		preamble	: in boolean := true) -- if true -> prepend preamble
		return string;
	
	--log_level : type_log_level := type_log_level'first;

	log_level_cmd_line_max : constant type_log_level := 15;
	subtype type_log_level_cmd_line is type_log_level range 0..log_level_cmd_line_max;

	-- This global variable is set on launching ET. See et.adb. It receives its
	-- value via the command line. It is a subtype of type_log_level and thus
	-- limited to a reasonable value.
	log_level : type_log_level_cmd_line := type_log_level_cmd_line'first;

	-- The log indentation is a global variable that serves to shift log messages
	-- to the right.
	log_indentation_max : constant positive := 30;
	type type_indentation_level is range 0..log_indentation_max;
	log_indentation : type_indentation_level := type_indentation_level'first;
	
	procedure log_indentation_up;
	procedure log_indentation_down;
	procedure log_indentation_reset;

	tabulator : constant character := latin_1.ht;
	
	function indent (width : in type_indentation_level) return string;

	type type_message_importance is (NORMAL, NOTE, WARNING, ERROR);

	-- Writes the given text with the current log_indentation in the current output. 
	-- If the system wide log level is greater or equal the the given log_level the given text is put on the log.
	-- Does not log anything if given level is no_logging.
	procedure log (
		importance	: in type_message_importance := NORMAL;
		text		: in string;
		level		: in type_log_level := type_log_level'first;
		console		: in boolean := false);

	


	
-- DATE
	date_characters : character_set := to_set (span => ('0','9')) or to_set ("-:T");
	type type_date is new string (1..19); -- "2017-08-17T14:17:25"

	function to_string (date : in type_date) return string;
	-- Returns the given date as string.

	function date_valid (date : in type_date) return boolean;
	-- Returns true if given date is valid and plausible.
	
	function date (preamble : in boolean := true) return string;
	-- Returns the current date as string in the format YYYY-MM-DDTHH:MM:SS

	function date_first return time; -- returns 1901-01-01
	-- Use it to indicate uninialized date.
	
	row_separator_length 	: constant positive := 100;
	row_separator_single	: constant string (1..row_separator_length)	:= row_separator_length * "-";	
	row_separator_double	: constant string (1..row_separator_length)	:= row_separator_length * "=";
-- 	item_not_specified		: constant string (1..7) := "missing";
	
-- WARNING AND ERROR MESSAGES
	function metric_system return string;
	-- Returns a message about the metric system used.

	function angles_in_degrees return string;
	-- Returns a message about the degrees used.
	
	function message_warning return string;
	-- Returns a warning string and increments the import/export) warning counter.

	function message_note return string;
	-- Returns a notification string.

	function strip_directory_separator (text : in string) return string;
	-- Removes a possible trailing directory separtor.
	
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

	-- Adds heading and trailing quotate to given string. 
	-- NOTE: apostrophe is ', quotation is "
	function enclose_in_quotes (
		text_in	: in string;
		quote	: in character := latin_1.apostrophe) 
		return string;

	-- Adds heading and trailing quotate to given character.
	-- NOTE: apostrophe is ', quotation is "
	function enclose_in_quotes (
		charcter_in	: in character;
		quote		: in character := latin_1.apostrophe) 
		return string;


	function trim_space_in_string (text_in : in string) return string;
	-- shrinks successive space characters to a single one in given string

	function remove_trailing_directory_separator (path_in : string) return string;
	-- removes a trailing directory separator.
	
	procedure write_message (
		file_handle	: in ada.text_io.file_type;
		identation 	: in natural := 0;
		text 		: in string;
		lf   		: in boolean := true;
		file 		: in boolean := true;
		console 	: in boolean := false);

	function is_number (text : in string) return boolean;
	-- Returns true if given string is a number. 
	
	function get_field_from_line (
	-- Extracts a field separated by ifs at position. If trailer is true, the 
	-- trailing content until trailer_to is also returned.
		text_in 	: in string;
		position 	: in positive;
		ifs 		: in character := latin_1.space;
		trailer 	: in boolean := false;
		trailer_to 	: in character := latin_1.semicolon
		) return string;

	-- This type serves to collect strings. It MUST be a vector, because
	-- this allows do pick out arbitrary strings by their indexes:
	package type_list_of_strings is new indefinite_vectors (
		index_type		=> positive, 
		element_type	=> string);

	-- This type is required when reading lines from files. It is a composite type
	-- whose components are hidden. The can only be accessed by special functions and procedures. See below.
	type type_fields_of_line is private;

	function read_line (
	-- Breaks down a given string and returns a type_fields_of_line.
		line			: in string; 									-- the line to be broken down
		number			: in positive_count := positive_count'first;	-- the line number	
		comment_mark	: in string; 						-- the comment mark like "--" or "#"
		test_whole_line	: in boolean := true; 				-- when false, cares for the comment mark at line begin only
															-- further comment marks are ignored
		ifs				: in character := latin_1.space;	-- field separator
		delimiter_wrap	: in boolean := false; 				-- true if text in delimiters is to be wrapped into a single field
		delimiter		: in character := latin_1.quotation	-- the text delimiter sign (mostly ")
		) return type_fields_of_line;

	-- Appends a field to a line:
	procedure append (
		line	: in out type_fields_of_line;
		field	: in string);
	
	-- Append right fields to left fields:
	function append (
		left	: in type_fields_of_line;
		right	: in type_fields_of_line) 
		return type_fields_of_line;

	-- Remove fields from line:
	function remove (
		line	: in type_fields_of_line;
		first	: in positive;
		last	: in positive)
		return type_fields_of_line;
					 
	procedure set_field (
		line		: in out type_fields_of_line;
		position	: in positive;
		content		: in string);
	
	-- Returns the field at the given position. Raises constraint error if there is no 
	-- field at given position.
	function get_field (
		line		: in type_fields_of_line;
		position	: in count_type) 
		return string;

	function to_string (line : in type_fields_of_line) return string;

	function line_number (line : in type_fields_of_line) return positive;
	-- Returns the line number of the given line.
	
	function affected_line (line : in type_fields_of_line) return string;
	-- Returns the line number of the given line in a string like "line x:"

	function field_count (line : in type_fields_of_line) return count_type;
	-- Returns the number of fields in the given line.

	function lines_equally (left, right : in type_fields_of_line) return boolean;

	report_handle : ada.text_io.file_type; -- CS rename to log_handle
	
	type type_warning_counter is private;

	procedure increment_warning_counter;
	-- Increments the warning counter by one.

	function warning_count return type_warning_counter;
	-- Returns the number of warnings.
	
	function warning_count return string;
	-- Returns the number of warnings as string.

	function no_warnings return boolean;
	-- Returns true if no warnings have been generated.

	function log_file_name return string;
	-- Returns the relative path and name of the import report file.
	
	procedure create_report;
	-- Creates the report file in report_directory.
	-- Sets the output to the report file.
	-- Leaves the report file open for further puts.

	procedure close_report;
	-- Writes the report footer and closes the report file.
	-- Sets the output back to standard_output.

	procedure show_line (
	-- Output the line of code where the exception occured:
		file : string; -- the file name like et_kicad.adb
		line : natural);-- the line number
	
	private
	
	type type_warning_counter is new natural;
		
	warning_counter : type_warning_counter := 0;
	
	type type_fields_of_line is record
		fields		: type_list_of_strings.vector;
		field_count	: count_type := count_type'first; -- number of fields in line
		number		: positive_count := positive_count'first; -- line numer
	end record;


		
end et_string_processing;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
