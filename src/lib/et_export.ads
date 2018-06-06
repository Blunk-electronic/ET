------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET EXPORT                               --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               S p e c                                    --
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

with ada.text_io;				use ada.text_io;
with ada.directories;			use ada.directories;

with et_general;
with et_string_processing;		--use et_string_processing;

package et_export is


	function file_report_export return string;
	-- Returns the relative path and name of the export report file.

	type type_warning_counter is private;
	
	procedure increment_warning_counter;
	-- Increments the warning counter by one.

	function warning_count return string;
	-- Returns the number of warnings as string.

	
	report_handle : ada.text_io.file_type;


	
	-- CAD FORMATS
	type type_cad_format is (et_v1, kicad_v4);
	-- If no format specified via cmd line, a default applies so that the operator can be 
	-- notified about missing cad format.
	cad_format : type_cad_format := type_cad_format'first; 

	procedure create_report;
	-- Creates the report file in report_directory.
	-- Sets the output to the report file.
	-- Leaves the report file open for further puts.

	procedure close_report;
	-- Writes the report footer and closes the report file.
	-- Sets the output back to standard_output.

	directory_cam			: constant string (1 ..  3) := "CAM";
	directory_statistics	: constant string (1 .. 10) := "statistics";
	
	procedure create_project_directory (
	-- Creates given project directory in work_directory of ET.
	-- Creates subdirectory for CAM
		project			: in string;
		log_threshold	: in et_string_processing.type_log_level);

	private
		type type_warning_counter is new natural;
		
		warning_counter : type_warning_counter := 0;
	
end et_export;

-- Soli Deo Gloria
