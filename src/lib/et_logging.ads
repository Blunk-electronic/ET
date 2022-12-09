-- ---------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           MESSAGE LOGGING                                --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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
with et_string_processing;		use et_string_processing;

package et_logging is

	type type_log_category is (
		NORMAL,
		HIGH,
		INSANE);

	log_category_default : constant type_log_category := NORMAL;
	
	function to_string (cat : in type_log_category) return string;
	function to_log_category (cat : in string) return type_log_category;
	
	
	-- The log level is limited to a reasonable value.
	log_level_max : constant positive := 50; -- CS increase if neccessary
	-- Functions and procedures pass each other this log level type.
	-- Theoretically it may assume indefinite values (for example during creation of 
	-- routing tables). However, it is limited to a reasonable value. See above.
	type type_log_level is range 0 .. log_level_max;

-- 	no_logging : constant type_log_level := type_log_level'last;
	
	-- Returns the given log level as string. 
	function to_string (
		log_level	: in type_log_level;
		preamble	: in boolean := true) -- if true -> prepend preamble
		return string;
	
	--log_level : type_log_level := type_log_level'first;

	log_level_cmd_line_max : constant type_log_level := 20;
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

	-- Writes the given text with the current log_indentation 
	-- in the current output. 
	-- If the system wide log level is greater or equal the 
	-- given log_level, then given text is put on the log.
	-- Does not log anything if given level is no_logging.
	-- Counts warnings independent of log level.
	procedure log (
		importance	: in type_message_importance := NORMAL;
		text		: in string;
		level		: in type_log_level := type_log_level'first;
		console		: in boolean := false);

	


	-- Returns a warning string and increments the import/export) warning counter.
	function message_warning return string;

	-- Returns a notification string.
	function message_note return string;

	
	procedure write_message (
		file_handle	: in ada.text_io.file_type;
		identation 	: in natural := 0;
		text 		: in string;
		lf   		: in boolean := true;
		file 		: in boolean := true;
		console 	: in boolean := false);


	report_handle : ada.text_io.file_type; -- CS rename to log_handle
	


	type type_warning_counter is new natural;
		
	warning_counter : type_warning_counter := 0;
	

	-- Increments the warning counter by one.
	procedure increment_warning_counter;

	
	-- Returns the number of warnings.
	function warning_count return type_warning_counter;

	
	-- Returns the number of warnings as string.
	function warning_count return string;

	
	-- Returns true if no warnings have been generated.
	function no_warnings return boolean;

	
	-- Returns the relative path and name of the import report file.
	function log_file_name return string;
	
	-- Creates the report file in report_directory.
	-- Sets the output to the report file.
	-- Leaves the report file open for further puts.
	procedure create_report;

	
	-- Writes the report footer and closes the report file.
	-- Sets the output back to standard_output.
	procedure close_report;

	
	-- Output the file and line:
	procedure show_line (
		file : string; -- the file name
		line : natural);-- the line number
	

		
end et_logging;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
