------------------------------------------------------------------------------
--                                                                          --
--                         SYSTEM ET IMPORT                                 --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2018 Mario Blunk, Blunk electronic                 --
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

with ada.directories;

with et_general;
with et_schematic;
with et_string_processing;			use et_string_processing;

package body et_import is

	function file_report_import return string is
	-- Returns the relative path and name of the import report file.
	begin
		return compose ( 
			containing_directory => compose (work_directory, report_directory),
			name => "import",
			extension => report_extension
			);
	end file_report_import;

	procedure validate_cad_format (format : in string) is
		use et_string_processing;
	begin
		-- CS: use a loop to probe formats
		if format = to_lower (type_cad_format'image (kicad_v4)) then
			null;
		else
			log_indentation_reset;
			log (message_error & "CAD format '"
					& format & "' invalid !" 
					& " Supported formats: "
					& type_cad_format'image (kicad_v4) -- CS: use a loop to offer formats
					& " !",
				console => true);
			raise constraint_error;
		end if;
	end validate_cad_format;

	function to_cad_format (format : in string) return type_cad_format is
	begin
		return type_cad_format'value (format);
	end to_cad_format;

	function to_string (format : in type_cad_format) return string is
	-- Converts the given cad format to a string.
	begin
		return type_cad_format'image (format);
	end to_string;

	function invalid_cad_format (format : in type_cad_format) return string is
	-- Returns a message that the given format is not supported.
	begin
		return "CAD format '" & to_string (format) & "' not supported or invalid !";
	end invalid_cad_format;
	
	procedure validate_project (
		name : in type_project_name.bounded_string;
		cad_format : in type_cad_format := UNKNOWN) is
	-- Checks if the given project of the given format exists in the current working directory.
	-- CS: currently this is just a test, whether the directory "name" exists.
	-- CS: do a more detailled check depending on cad format (look for project files).
	begin
		if exists (type_project_name.to_string (name)) then
			null; -- fine
		else
			log_indentation_reset;
			log (message_error & "project '" & type_project_name.to_string (name) 
				& "' not found ! Working directory correct ?",
				console => true);
			raise constraint_error;
		end if;
	end validate_project;
	
	procedure create_report is
	-- Creates the report file in report_directory.
	-- Sets the output to the report file.
	-- Leaves the report file open for further puts.
    begin
		create (file => et_import.report_handle, mode => out_file, 
				name => file_report_import);

		set_output (et_import.report_handle);
		
		put_line (system_name & " " & version & " import report");
		put_line (date);
		put_line (metric_system);
		put_line (angles_in_degrees);
		put_line (to_string (log_level));
		put_line (row_separator_double);		
	end create_report;

	procedure close_report is
	-- Writes the report footer and closes the report file.
	-- Sets the output back to standard_output.
	begin
		if is_open (et_import.report_handle) then
	
			put_line (row_separator_double);
			
			if warning_counter = 0 then
				put_line ("no warnings");
			else
				put_line ("warnings" & type_warning_counter'image (warning_counter));
			end if;
			
			put_line (row_separator_single);
			
			put_line (date);
			put_line (system_name & " import report end");

			set_output (standard_output);
			
			close (et_import.report_handle);

			if warning_counter > 0 then
				put_line (standard_output, "WARNING ! "
					& "Read import report " & file_report_import & " for warnings and error messages !");
			end if;
			
		end if;
	end close_report;

end et_import;

-- Soli Deo Gloria
