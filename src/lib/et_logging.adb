-- ---------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           MESSAGE LOGGING                                --
--                                                                          --
--                               B o d y                                    --
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

with ada.strings;				use ada.strings;
with ada.strings.unbounded; 	use ada.strings.unbounded;

--with ada.exceptions;
with gnat.source_info;
with gnat.calendar;

package body et_logging is

	function to_string (cat : in type_log_category) return string is begin
		return type_log_category'image (cat);
	end to_string;

	
	function to_log_category (cat : in string) return type_log_category is begin
		return type_log_category'value (cat);
	end to_log_category;


	
	function to_string (
		log_level	: in type_log_level;
		preamble	: in boolean := true) -- if true -> prepend preamble
		return string is
	begin
		if preamble then
			return "log level" & type_log_level'image (log_level);
		else
			return trim (type_log_level'image (log_level), left);
		end if;
	end to_string;

	
	procedure log_indentation_up is
	begin
		log_indentation := log_indentation + 1;
		exception
			when constraint_error =>
				put_line ("WARNING ! Maximum log indentation reached !");
				log_indentation := type_indentation_level'last;
			when others => null;
	end log_indentation_up;

	
	procedure log_indentation_down is
	begin
		log_indentation := log_indentation - 1;
		exception
			when constraint_error =>
				put_line ("WARNING ! Minimum log indentation reached !");
				log_indentation := type_indentation_level'first;
			when others => null;
	end log_indentation_down;

	
	procedure log_indentation_reset is
	begin
		log_indentation := type_indentation_level'first;
	end log_indentation_reset;

	
-- 	procedure log_indentation_operation (operation : in type_log_identation_operation) is
-- 	begin
-- 		null;
-- 	end log_indentation_operation;

	
	function indent (width : in type_indentation_level) return string is
	begin
		return (natural(width) * latin_1.space);
	end indent;

	
	procedure log (
		importance	: in type_message_importance := NORMAL;
		text		: in string;
		level		: in type_log_level := type_log_level'first;
		console		: in boolean := false) 
	is

		function to_importance (importance : in type_message_importance) return string is begin
			case importance is
				when NORMAL => return "";
				when others => return type_message_importance'image (importance) & ": ";
			end case;
		end;
		
		function write_text (indentation_on : in boolean := true) 
			return string is 

			fill : string := natural (log_indentation) * latin_1.space;
		begin
			if indentation_on then
				return fill & to_importance (importance) & text;
			else
				return to_importance (importance) & text;
			end if;
		end;
		
	begin -- log
-- 		if level < no_logging then
			
			if log_level >= level then

				case importance is
					when NORMAL =>
						put_line (report_handle, write_text);

						if console then
							put_line (standard_output, write_text);
						end if;

					when NOTE =>
						put_line (report_handle, write_text);

						if console then
							put_line (standard_output, write_text);
						end if;

					when WARNING =>
						increment_warning_counter;
						
						put_line (report_handle, write_text (false)); -- indentation off

						if console then
							put_line (standard_output, write_text (false)); -- indentation off
						end if;

					when ERROR =>
						put_line (report_handle, write_text (false)); -- indentation off

						if console then
							put_line (standard_output, write_text (false)); -- indentation off
						end if;
						
				end case;
				
			end if;

-- 		end if;	
	end log;

	
	
	function message_warning return string is
		warning : constant string (1..9) := "WARNING #";
	begin
		increment_warning_counter;
		return warning & trim (warning_count, left) & " : ";
	end message_warning;

	
	function message_note return string is
	begin
		return "NOTE : ";
	end message_note;
	



		
	procedure write_message (
		file_handle : in ada.text_io.file_type;
		identation : in natural := 0;
		text : in string; 
		lf   : in boolean := true;		
		file : in boolean := true;
		console : in boolean := false) is
	begin
		if file then
			put(file_handle, identation * ' ' & text);
			if lf then 
				new_line(file_handle);
			end if;
		end if;

		if console then
			put(standard_output,identation * ' ' & text);
			if lf then 
				new_line(standard_output);
			end if;
		end if;
	end write_message;

	



	procedure increment_warning_counter is begin
		warning_counter := warning_counter + 1;
	end increment_warning_counter;

	
	function warning_count return type_warning_counter is begin
		return warning_counter;
	end warning_count;

	
	function no_warnings return boolean is begin
		if warning_counter = 0 then return true;
		else return false;
		end if;
	end no_warnings;

	
	function warning_count return string is begin
		return type_warning_counter'image (warning_counter);
	end warning_count;

	function log_file_name return string is
		use et_general;
	begin
		return compose ( 
			containing_directory 	=> compose (work_directory, report_directory),
			name					=> "messages",
			extension				=> report_extension
			);
	end log_file_name;

	
	procedure create_report is
		use et_general;
		previous_output : ada.text_io.file_type renames current_output;
    begin
		create (file => report_handle,
				mode => out_file, 
				name => log_file_name);

		set_output (report_handle);
		
		put_line (system_name & " " & version & " messages log");
		put_line (date);
		put_line (metric_system);
		put_line (angles_in_degrees);
		put_line (row_separator_double);

		set_output (previous_output);
	end create_report;

	
	procedure close_report is
		use et_general;
	begin
		if is_open (report_handle) then

			set_output (report_handle);
			
			put_line (row_separator_double);

			if no_warnings then
				put_line ("no warnings");
			else
				put_line ("warnings" & warning_count);
			end if;
			
			put_line (row_separator_single);
			
			put_line (date);
			put_line (system_name & " log messages end");

			set_output (standard_output);
			
			close (report_handle);

			if not no_warnings then -- means if there are warnings
				put_line (standard_output, "WARNING ! "
					& "Read log file " & log_file_name & " for warnings and error messages !");
			end if;
			
		end if;
	end close_report;


	procedure show_line (
		file : string; -- the file name like et_kicad.adb
		line : natural) is -- the line number 
	begin
		log_indentation_reset;
		log (text => "source file " & file & " line" & natural'image (line), console => true);
	end;

	
end et_logging;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
