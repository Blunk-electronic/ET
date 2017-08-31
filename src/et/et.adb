------------------------------------------------------------------------------
--                                                                          --
--                         SYSTEM ET BASE                                   --
--                                                                          --
--                                 M-1                                      --
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

with ada.text_io;				use ada.text_io;
with ada.integer_text_io;		use ada.integer_text_io;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;

with ada.strings.bounded; 		use ada.strings.bounded;
with ada.exceptions; 			use ada.exceptions;
 
with ada.command_line;			use ada.command_line;
with gnat.command_line;			use gnat.command_line;
with ada.directories;			use ada.directories;

with et_general;	use et_general;
with et_import;		
with et_kicad;

procedure et is

	version : string (1..3) := "000";

	procedure get_commandline_arguments is
	begin
		loop 
			case getopt(switch_version 
						& latin_1.space & switch_help
						& latin_1.space & switch_log_level & latin_1.equals_sign
						--& latin_1.space & switch_import_file & latin_1.equals_sign -- CS: see below
						& latin_1.space & switch_import_project & latin_1.equals_sign
						& latin_1.space & switch_import_format & latin_1.equals_sign
					) is

				when latin_1.hyphen => -- which is a '-'
					if full_switch = switch_version then
						put_line (system_name & " version " & version);
						
					elsif full_switch = switch_help then
						put_line ("help"); -- CS write helpful help

--					CS: currently we do not care about importing single files
--						needs distinction between schematic and board file
-- 					elsif full_switch = switch_import_file then
-- 						put_line("import file " & parameter); 

					elsif full_switch = switch_import_project then
						put_line ("import project " & parameter);
						et_import.project_file_name := et_import.type_project_file_name.to_bounded_string (parameter);

					elsif full_switch = switch_import_format then
						put_line ("import format " & parameter);
						et_import.cad_format := et_import.type_cad_format'value (parameter);

					elsif full_switch = switch_log_level then
						put_line ("log level " & parameter);
						log_level := natural'value (parameter);
					end if;

					
				when others => exit; -- CS: produce useful message

			end case;
		end loop;
	end get_commandline_arguments;

	use et_import.type_project_file_name;
	use et_import;
begin

	get_commandline_arguments;

	-- Test if project file specified and if it exists:
	if length (et_import.project_file_name) > 0 then
		if exists (to_string(et_import.project_file_name)) then
			--put_line( "project file: " & to_string(et_import.project_file_name));
			null;
		else
			put_line (message_error & "project file " & to_string (et_import.project_file_name) 
				& " not found ! (forgot extension ?)");
			raise constraint_error;
		end if;
	else
		put_line (message_error & "project name not specified !");
		raise constraint_error;
	end if;

	-- Test if cad format specified:
	if et_import.cad_format = unknown then
		put_line (message_error & "CAD format not specified !");
		raise constraint_error;
	end if;		

	if not exists (report_directory) then
		put_line ("creating report directory ...");
		create_directory (report_directory);
	end if;

	-- Now we know the project file name and the CAD format.
	et_kicad.import_design;

end et;

-- Soli Deo Gloria
