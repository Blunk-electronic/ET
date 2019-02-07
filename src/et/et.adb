------------------------------------------------------------------------------
--                                                                          --
--                            SYSTEM ET BASE                                --
--                                                                          --
--                                 M-1                                      --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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

--   For correct displaying set tab width in your editor to 4.

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

--with gnat.source_info;

with et_general;
with et_string_processing;		use et_string_processing;
with et_coordinates;
with et_schematic;
with et_import;
with et_export;
with et_configuration;
with et_kicad;
with et_kicad_pcb;
with et_kicad_to_native;
with et_project;

procedure et is

	-- Depending on the command line arguments this variable tells what the operator wants to do:
	operator_action : et_general.type_operator_action := et_general.request_help;
	conf_file_name	: et_configuration.type_configuration_file_name.bounded_string;	

	module_name_import	: et_project.type_project_name.bounded_string; -- used for single module import
	
	procedure get_commandline_arguments is
		use et_schematic;
		use et_general;
		use et_project;
	begin
		loop 
			case getopt (switch_version -- FIND THE SWITCH STRINGS IN ET_GENERAL !!!
						& latin_1.space & switch_help -- no parameter
						& latin_1.space & switch_make_default_conf & latin_1.equals_sign
						& latin_1.space & switch_log_level & latin_1.equals_sign
						--& latin_1.space & switch_import_file & latin_1.equals_sign -- CS: see below
						& latin_1.space & switch_import_module & latin_1.equals_sign
						& latin_1.space & switch_import_format & latin_1.equals_sign
						--& latin_1.space & switch_import_modules -- no parameter
						& latin_1.space & switch_configuration_file & latin_1.equals_sign
						--& latin_1.space & switch_native_project & latin_1.equals_sign
						& latin_1.space & switch_native_project_open & latin_1.equals_sign						
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

					elsif full_switch = switch_import_module then
						put_line ("import module " & strip_directory_separator (parameter));
						module_name_import := et_project.type_project_name.to_bounded_string (parameter);

						-- set operator action
						operator_action := IMPORT_MODULE;

					elsif full_switch = switch_import_format then
						put_line ("import format " & parameter);
						et_import.cad_format := et_import.type_cad_format'value (parameter);

-- 					elsif full_switch = switch_import_modules then
-- 						put_line ("import modules as specified by configuraton file");
-- 
-- 						-- set operator action
-- 						operator_action := IMPORT_MODULES;
						
					elsif full_switch = switch_make_default_conf then -- make configuration file
						put_line ("configuration file " & parameter);

						-- set operator action
						operator_action := MAKE_CONFIGURATION;
						conf_file_name := et_configuration.type_configuration_file_name.to_bounded_string (parameter);

					elsif full_switch = switch_configuration_file then -- define configuration file
						put_line ("configuration file " & parameter);
						conf_file_name := et_configuration.type_configuration_file_name.to_bounded_string (parameter);

-- 					elsif full_switch = switch_native_project then
-- 						put_line ("native target project " & parameter);
-- 						project_name := et_project.to_project_name (parameter);

					elsif full_switch = switch_native_project_open then
						put_line ("native project " & parameter);
						project_name := et_project.to_project_name (remove_trailing_directory_separator (parameter));

						-- set operator action
						operator_action := OPEN_NATIVE_PROJECT;
						
					elsif full_switch = switch_log_level then
						put_line ("log level " & parameter);
						log_level := type_log_level_cmd_line'value (parameter);
					end if;

					
				when others => exit; -- CS: produce useful message

			end case;
		end loop;

		exception
			when others =>
				put_line (message_error & "command line argument error !");
				null;
				-- CS: show help and command line switches
				raise;
				
	end get_commandline_arguments;

	procedure backup_projects_root_directory is
		use et_project;
		use et_project.type_projects_root_dir;
	begin
		-- CS: log ?
		projects_root_dir := to_bounded_string (current_directory);
	end backup_projects_root_directory;

	procedure restore_projects_root_directory is
		use et_project;
		use et_project.type_projects_root_dir;
	begin
		log_indentation_reset;
		log (text => "changing back to projects directory " & to_string (projects_root_dir) & " ...",
			 level => 1);
		set_directory (to_string (projects_root_dir));
	end restore_projects_root_directory;
	
	procedure create_work_directory is  -- CS move to et_string_processing
		use et_general;
	begin
		if not exists (work_directory) then
			put_line ("creating " & system_name & " work directory " & work_directory & " ...");
			create_directory (work_directory);
		end if;
	end create_work_directory;

	procedure create_report_directory is -- CS move to et_string_processing
		use et_general;
	begin	
		if not exists (compose (work_directory, report_directory)) then
			put_line ("creating report directory ...");
			create_directory (compose (work_directory, report_directory));
		end if;
	end create_report_directory;

-- 	procedure test_if_native_project_specified is
-- 	-- Aborts program if no native target project name specified via cmd line parameter.
-- 		use et_project;
-- 		use et_project.type_project_name;
-- 	begin
-- 		if length (project_name) = 0 then
-- 			log_indentation_reset;
-- 			log (message_error & "Native target project name not specified !");
-- 			raise constraint_error;
-- 		end if;
-- 	end test_if_native_project_specified;
	
	procedure import_module is
	-- This imports a single module.
	-- CAUTION: uses the global variable module_name_import !!!
		use et_schematic;
		use et_project.type_project_name;
		use et_import;
	begin
		-- Test if project name specified and if project base directory exists:
		if length (module_name_import) > 0 then

			-- If project name was provided with a trailing directory separator it must be removed.
			module_name_import := et_project.to_project_name (strip_directory_separator (et_project.to_string (module_name_import)));
			validate_project (module_name_import, et_import.cad_format);
		else
			put_line (message_error & "project name not specified !");
			raise constraint_error;
		end if;

		-- Test if cad format specified:
		if et_import.cad_format = UNKNOWN then
			put_line (message_error & "CAD format not specified !");
			raise constraint_error;
		end if;		

		-- read configuration file if specified. otherwise issue warning
		if et_configuration.type_configuration_file_name.length (conf_file_name) > 0 then
			et_configuration.read_configuration (
				file_name => conf_file_name,
				--single_module => true, -- we are dealing a single project
				log_threshold => 0);
		else
			log (message_warning & "no configuration file specified !");
		end if;
		
		-- The design import requires changing of directories. So we backup the current directory.
		-- After the import, we restore the directory.
		backup_projects_root_directory;

		log ("importing module " & et_project.to_string (module_name_import) & " ...", console => true);
		log ("CAD format " & to_string (et_import.cad_format));
				
		case et_import.cad_format is
			when et_import.KICAD_V4 | et_import.KICAD_V5 =>
				et_kicad.import_design (project => module_name_import, log_threshold => 0);
				
			when others => -- CS
				raise constraint_error;
		end case;
		
		restore_projects_root_directory;

		exception
			when event:
				others =>
					put_line (standard_output, message_error & "Read import report for warnings and error messages !"); -- CS: show path to report file
					raise;

	end import_module;

	procedure read_boards is
		--use et_schematic;
		use et_kicad.type_modules;
		use et_configuration;
	begin
		case et_import.cad_format is
			when et_import.KICAD_V4 | et_import.KICAD_V5 =>
		
				-- If there are no modules, there is nothing to check:
				if et_kicad.module_count > 0 then

					log (row_separator_double);
					log ("importing layouts/boards ...", console => true);
					
					log_indentation_up;

					et_kicad_pcb.read_boards (log_threshold => 0);
					
					log_indentation_down;
				end if;

			when others =>
				raise constraint_error; -- CS
		end case;
	end read_boards;


	procedure convert is
	begin
		log (et_string_processing.row_separator_single);
		log ("converting to " & et_general.system_name & " native project ...", console => true);
		log_indentation_up;
		
		case et_import.cad_format is
			when et_import.KICAD_V4 | et_import.KICAD_V5 =>
				et_kicad_to_native.to_native (log_threshold => 0);
				--et_kicad_pcb.to_native (log_threshold => 0);
				
			when others =>
				raise constraint_error; -- CS
		end case;

		log_indentation_down;
	end convert;

	
begin -- main
	create_work_directory;
	create_report_directory;
	create_report;
	
	-- process command line arguments
	get_commandline_arguments;

	case operator_action is
		when et_general.REQUEST_HELP =>
			null; -- CS

		when et_general.MAKE_CONFIGURATION =>
			et_configuration.make_default_configuration (conf_file_name, log_threshold => 0);

		when et_general.IMPORT_MODULE =>

			-- import a single module indicated by variable module_name_import
			import_module; -- calls import_design (according to CAD format) -- CS rename to import_project ?

			log_indentation_reset;
			read_boards; -- writes in import report. closes import report

			convert;
			
			
-- 		when et_general.IMPORT_MODULES =>
-- 			-- The targeted native ET project must be specified via cmd line parameter:
-- 			test_if_native_project_specified;
-- 
-- 			-- import many modules as specified in configuration file
-- 			import_modules; -- calls import_design (according to CAD format)
-- 
-- 			-- check modules
-- 			check_modules; -- updates the netlists of all modules. creates and opens export report
-- 
-- 			log_indentation_reset;
-- 			read_boards; -- writes in import report. closes import report
-- 
-- 			convert;


		when et_general.OPEN_NATIVE_PROJECT =>
			
			-- read configuration file if specified. otherwise issue warning
			if et_configuration.type_configuration_file_name.length (conf_file_name) > 0 then
				et_configuration.read_configuration (
					file_name		=> conf_file_name,
					log_threshold	=> 0);
			else
				log (message_warning & "no configuration file specified !");
			end if;

			-- open specified project
			et_project.open_project (log_threshold => 0);
	end case;

	close_report;

	exception
		when event: others =>
			log_indentation_reset;
			log (ada.exceptions.exception_information (event), console => true);
			close_report;
			
			put_line ("Read log file " & log_file_name & " for details !");
			set_exit_status (failure);

end et;

-- Soli Deo Gloria
