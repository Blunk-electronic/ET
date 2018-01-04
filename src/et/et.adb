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

--   For correct displaying set tab with in your editor to 4.

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
with ada.integer_text_io;		use ada.integer_text_io;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;

with ada.strings.bounded; 		use ada.strings.bounded;
with ada.exceptions; 			use ada.exceptions;
 
with ada.command_line;			use ada.command_line;
with gnat.command_line;			use gnat.command_line;
with ada.directories;			use ada.directories;

with et_general;				use et_general;
with et_string_processing;		use et_string_processing;
with et_schematic;
with et_import;
with et_export;
with et_kicad;
with et_netlist;

procedure et is
	
	procedure get_commandline_arguments is
		use et_schematic;
	begin
		loop 
			case getopt (switch_version 
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
						put_line ("import project " & strip_directory_separator (parameter));
						project_name := type_project_name.to_bounded_string (parameter);

					elsif full_switch = switch_import_format then
						put_line ("import format " & parameter);
						et_import.cad_format := et_import.type_cad_format'value (parameter);

					elsif full_switch = switch_log_level then
						put_line ("log level " & parameter);
						log_level := type_log_level'value (parameter);
					end if;

					
				when others => exit; -- CS: produce useful message

			end case;
		end loop;
	end get_commandline_arguments;

	procedure backup_projects_root_directory is
		use et_schematic;
		use et_schematic.type_projects_root_dir;
	begin
		-- CS: log ?
		projects_root_dir := to_bounded_string (current_directory);
	end backup_projects_root_directory;

	procedure restore_projects_root_directory is
		use et_schematic;
		use et_schematic.type_projects_root_dir;
	begin
		log_indentation_reset;
		log (text => "changing back to projects directory '" & to_string (projects_root_dir) & "' ...",
			 level => 1);
		set_directory (to_string (projects_root_dir));
	end restore_projects_root_directory;
	
	procedure create_work_directory is
	begin
		if not exists (work_directory) then
			put_line ("creating " & system_name & " work directory '" & work_directory & "' ...");
			create_directory (work_directory);
		end if;
	end create_work_directory;

	procedure create_report_directory is
	begin	
		if not exists (compose (work_directory, report_directory)) then
			put_line ("creating report directory ...");
			create_directory (compose (work_directory, report_directory));
		end if;
	end create_report_directory;
	
	procedure import_design is
		use et_schematic;
		use et_schematic.type_project_name;
		use et_import;
	begin
		-- Test if project name specified and if project base directory exists:
		if length (project_name) > 0 then

			-- If project name was provided with a trailing directory separator it must be removed.
			project_name := to_bounded_string (strip_directory_separator (to_string (project_name)));
			
			if exists (to_string (project_name)) then
				--put_line( "project file: " & to_string(et_import.project_file_name));
				null; -- fine
			else
				put_line (message_error & "project '" & to_string (project_name) 
					& "' not found ! (working directory correct ?)");
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

		create_work_directory;
		create_report_directory;
		et_import.create_report; -- directs all puts to the report file
		
		-- The design import requires changing of directories. So we backup the current directory.
		-- After the import, we restore the directory.
		backup_projects_root_directory;
		et_kicad.import_design (log_threshold => 0);
		restore_projects_root_directory;

		et_import.close_report;

		exception
			when event:
				constraint_error =>
					et_import.close_report;
					put_line (standard_output, message_error & "Read import report for warnings and error messages !"); -- CS: show path to report file
					raise;

	end import_design;
	
begin -- main

	-- process command line arguments
	get_commandline_arguments;

	-- import design indicated by variable project_name
	import_design;

	-- export useful things from the imported project(s)
	et_export.create_report;
	reset_warnings_counter;

	-- detect missing or orphaned junctions
	et_schematic.check_junctions (log_threshold => 0);
	et_schematic.check_orphaned_junctions (log_threshold => 0);
	et_schematic.check_misplaced_junctions (log_threshold => 0);	

	-- detect misplaced no-connect-flags
	et_schematic.check_misplaced_no_connection_flags (log_threshold => 0);

	-- detect orphaned no-connect-flags
	et_schematic.check_orphaned_no_connection_flags (log_threshold => 0);

	-- make netlists
	et_schematic.make_netlists (log_threshold => 0);

	-- detect unintentionally left open ports (must happen AFTER make_netlists !)
	et_schematic.check_open_ports (log_threshold => 0);

	-- detect non-deployed units
	et_schematic.check_non_deployed_units (log_threshold => 0);

	-- test nets for inputs, outputs, bidirs, ...
	et_schematic.net_test (log_threshold => 0);
	
	-- export netlists (requires that make_netlists has been called previously)
	et_schematic.export_netlists (log_threshold => 0);
	
	-- export statistics
	et_schematic.write_statistics (log_threshold => 0);

	-- export bom
	et_schematic.export_bom (log_threshold => 0);
	
	et_export.close_report;

	exception
		when event:
			constraint_error => 
				et_export.close_report;
				put_line (standard_output, message_error & "Read export report for warnings and error messages !"); -- CS: show path to report file
				set_exit_status (failure);

end et;

-- Soli Deo Gloria
