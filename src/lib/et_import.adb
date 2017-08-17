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

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

--with ada.characters;			use ada.characters;
--with ada.characters.latin_1;	use ada.characters.latin_1;
--with ada.characters.handling;	use ada.characters.handling;
-- with ada.text_io;				use ada.text_io;
-- with ada.strings; 				use ada.strings;
-- with ada.strings.fixed; 		use ada.strings.fixed;
-- with ada.strings.bounded; 		use ada.strings.bounded;
-- with ada.strings.unbounded; 	use ada.strings.unbounded;
-- with ada.numerics.real_arrays;  use ada.numerics.real_arrays;
-- with ada.directories;			use ada.directories;
-- with ada.exceptions; 			use ada.exceptions;
-- 
-- with et_schematic;				use et_schematic;
-- 
-- with et_geometry;				use et_geometry;
-- 
with et_string_processing;			use et_string_processing;

package body et_import is

	procedure create_report is
	-- Creates the report file in report_directory.
	-- Sets the output to the report file.
	-- Leaves the report file open for further puts.
    begin
		create (file => et_import.report_handle, mode => out_file, 
				name => file_report_import);

		set_output(et_import.report_handle);
		
		put_line(system_name & " import report");
		put_line("date " & string(date_now));
		put_line("CAD format " & type_cad_format'image(cad_format));
		put_line("project file " & to_string(et_import.project_file_name));
		put_line(row_separator_double);		
	end create_report;

	procedure close_report is
	-- Writes the report footer and closes the report file.
	-- Sets the output back to standard_output.
	begin
		put_line(row_separator_double);
		put_line("date " & string(date_now));
		put_line(system_name & " import report end");

		set_output(standard_output);
		
		close(et_import.report_handle);
		
	end close_report;

end et_import;

-- Soli Deo Gloria
