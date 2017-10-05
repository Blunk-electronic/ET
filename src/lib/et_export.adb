------------------------------------------------------------------------------
--                                                                          --
--                         SYSTEM ET EXPORT                                 --
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

package body et_export is

	function file_report_export return string is
	-- Returns the relative path and name of the export report file.
		use et_general;
	begin
		return compose ( 
			containing_directory => compose (work_directory, report_directory),
			name => "export",
			extension => report_extension
			);
	end file_report_export;

	procedure create_report is
	-- Creates the report file in report_directory.
	-- Sets the output to the report file.
	-- Leaves the report file open for further puts.
    begin
		create (file => et_export.report_handle, mode => out_file, 
				name => file_report_export);

		set_output (et_export.report_handle);
		
		put_line (et_general.system_name & " export report");
		put_line ("date " & string (date_now));
		put_line ("CAD format " & type_cad_format'image (cad_format));
		put_line ("project file " & et_schematic.type_project_file_name.to_string (et_schematic.project_file_name));
		put_line ("CAUTION: Measurement system is METRIC. All dimensions given in Millimeters !");
		put_line ("CAUTION: All angles are given in degrees !");
		put_line ("log level" & type_log_level'image (log_level));
		put_line (row_separator_double);
		log_indentation_reset;
	end create_report;

	procedure close_report is
	-- Writes the report footer and closes the report file.
	-- Sets the output back to standard_output.
	begin
		put_line(row_separator_double);
		
		if warning_counter = 0 then
			put_line ("no warnings");
		else
			put_line ("warnings" & type_warning_counter'image (warning_counter));
		end if;
		
		put_line(row_separator_single);
		
		put_line ("date " & string(date_now));
		put_line (et_general.system_name & " export report end");

		set_output(standard_output);
		
		close (et_export.report_handle);
		
	end close_report;

	procedure create_project_directory (project : in string) is
	-- Creates given project directory in work_directory of ET.
		use et_general;
	begin
		if not exists (compose (work_directory, project)) then
			log (
				text => "creating project directory '" & compose (work_directory, project) & "' ...",
				level => 2);

			create_directory (compose (work_directory, project));
		end if;
	end create_project_directory;

	
end et_export;

-- Soli Deo Gloria
