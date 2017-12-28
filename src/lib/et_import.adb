------------------------------------------------------------------------------
--                                                                          --
--                         SYSTEM ET IMPORT                                 --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               B o d y                                    --
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
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

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
	
	procedure create_report is
	-- Creates the report file in report_directory.
	-- Sets the output to the report file.
	-- Leaves the report file open for further puts.
    begin
		create (file => et_import.report_handle, mode => out_file, 
				name => file_report_import);

		set_output (et_import.report_handle);
		
		put_line (system_name & " import report");
		put_line ("project " & et_schematic.type_project_name.to_string (et_schematic.project_name)); -- CS: function that returns the project name
		put_line ("date " & string (date_now));
		put_line ("CAD format " & type_cad_format'image (cad_format));
		put_line ("CAUTION: Measurement system is METRIC. All dimensions given in millimeters !"); -- CS: function that returns this text
		put_line ("CAUTION: All angles are given in degrees !"); -- CS: function that returns this text
		put_line ("log level" & type_log_level'image (log_level)); -- CS: function that returns the log level
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
			
			put_line ("date " & string (date_now));
			put_line (system_name & " import report end");

			set_output (standard_output);
			
			close (et_import.report_handle);

			if warning_counter > 0 then
				put_line (standard_output, "Read import report for warnings and error messages !"); -- CS: show path to report file
			end if;
-- 			put_line (standard_output, "successful !");
-- 			put_line ("Read import and export reports for warnings and error messages !"); -- CS: show path to report file
		end if;
	end close_report;

end et_import;

-- Soli Deo Gloria
