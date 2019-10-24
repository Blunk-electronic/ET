------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                                BASE                                      --
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

--   The two letters "CS" indicate a "construction site" where things are not
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
with et_packages;
with et_schematic;
with et_import;
with et_export;
with conventions;
with et_kicad;
with et_kicad_pcb;
with et_kicad_to_native;
with et_project;
with scripting;

with et_packages;
with pcb_rw;

procedure et is

	conv_file_name_create	: conventions.type_conventions_file_name.bounded_string;
	conv_file_name_use		: conventions.type_conventions_file_name.bounded_string;

	project_name_create		: et_project.type_project_name.bounded_string; -- the project to be created
	project_name_import		: et_project.type_project_name.bounded_string; -- the project to be imported
	project_name_open 		: et_project.type_project_name.bounded_string; -- the project to be opened
	project_name_save_as	: et_project.type_project_name.bounded_string; -- the "save as" name of the project
	
	package_name_create		: et_packages.type_package_model_file.bounded_string; -- the package to be created like libraries/packages/S_SO14.pac
	package_name_import		: et_packages.type_package_model_file.bounded_string; -- the package to be imported
	package_name_open		: et_packages.type_package_model_file.bounded_string; -- the package to be opened
	package_name_save_as	: et_packages.type_package_model_file.bounded_string; -- the package to be saved as
	package_appearance		: et_packages.type_package_appearance := et_packages.REAL; -- virtual/real. mostly real.
	
	script_name	: scripting.type_script_name.bounded_string;
	
	procedure get_commandline_arguments is
		use et_schematic;
		use et_general;
		use et_project;

		arg : constant string := ("argument: -");
		space : character renames latin_1.space;
	begin
		loop 
			case getopt (switch_version -- FIND THE SWITCH STRINGS IN ET_GENERAL !!!
						& latin_1.space & switch_help -- no parameter
						& latin_1.space & switch_make_default_conv & latin_1.equals_sign
						& latin_1.space & switch_log_level & latin_1.equals_sign
						& latin_1.space & switch_import_project & latin_1.equals_sign
						& latin_1.space & switch_import_format & latin_1.equals_sign
						& latin_1.space & switch_conventions & latin_1.equals_sign
						
						& latin_1.space & switch_native_project_create & latin_1.equals_sign
						& latin_1.space & switch_native_project_open & latin_1.equals_sign
						& latin_1.space & switch_native_project_save_as & latin_1.equals_sign

						& latin_1.space & switch_native_package_create & latin_1.equals_sign
						& latin_1.space & switch_package_appearance & latin_1.equals_sign
						& latin_1.space & switch_native_package_open & latin_1.equals_sign						
						& latin_1.space & switch_native_package_save_as & latin_1.equals_sign
						
						& latin_1.space & switch_execute_script & latin_1.equals_sign
					) is

				when latin_1.hyphen => -- which is a '-'
					if full_switch = switch_version then
						put_line (system_name & " version " & version);
						
					elsif full_switch = switch_help then
						put_line ("help"); -- CS write helpful help

					elsif full_switch = switch_import_project then
						log (text => arg & full_switch & space & strip_directory_separator (parameter));
						project_name_import := et_project.type_project_name.to_bounded_string (parameter);

					elsif full_switch = switch_import_format then
						log (text => arg & full_switch & space & parameter);
						et_import.cad_format := et_import.type_cad_format'value (parameter);

					elsif full_switch = switch_make_default_conv then -- make conventions file
						log (text => arg & full_switch & space & parameter);
						conv_file_name_create := conventions.type_conventions_file_name.to_bounded_string (parameter);

					elsif full_switch = switch_conventions then -- use conventions file
						log (text => arg & full_switch & space & parameter);
						conv_file_name_use := conventions.type_conventions_file_name.to_bounded_string (parameter);

						
					elsif full_switch = switch_native_project_create then
						log (text => arg & full_switch & space & parameter);
						project_name_create := et_project.to_project_name (remove_trailing_directory_separator (parameter));
						
					elsif full_switch = switch_native_project_open then
						log (text => arg & full_switch & space & parameter);
						project_name_open := et_project.to_project_name (remove_trailing_directory_separator (parameter));

					elsif full_switch = switch_native_project_save_as then
						log (text => arg & full_switch & space & parameter);
						project_name_save_as := et_project.to_project_name (remove_trailing_directory_separator (parameter));


					elsif full_switch = switch_native_package_create then
						log (text => arg & full_switch & space & parameter);
						package_name_create := et_packages.to_file_name (parameter); -- libraries/packages/smd/SOT23.pac

					elsif full_switch = switch_package_appearance then -- virtual/real
						log (text => arg & full_switch & space & parameter);
						package_appearance := et_packages.to_appearance (parameter);
						
					elsif full_switch = switch_native_package_open then
						log (text => arg & full_switch & space & parameter);
						package_name_open := et_packages.to_file_name (parameter); -- libraries/packages/smd/SOT23.pac

					elsif full_switch = switch_native_package_save_as then
						log (text => arg & full_switch & space & parameter);
						package_name_save_as := et_packages.to_file_name (parameter); -- libraries/packages/smd/SOT23.pac

						
					elsif full_switch = switch_execute_script then
						log (text => arg & full_switch & space & parameter);
						script_name := scripting.to_script_name (parameter);
						
					elsif full_switch = switch_log_level then
						log (text => arg & full_switch & space & parameter);
						log_level := type_log_level_cmd_line'value (parameter);
					end if;

					
				when others => 
-- 					show_cdl_switches; -- CS
					exit;
-- 					raise constraint_error;

			end case;
		end loop;

		exception
			when others =>
				put_line (message_error & "command line argument error !");
				show_cdl_switches;
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
	
	procedure create_work_directory is
		use et_general;
	begin
		if not exists (work_directory) then
			put_line ("creating " & system_name & " work directory " & work_directory & " ...");
			create_directory (work_directory);
		end if;
	end create_work_directory;

	procedure create_report_directory is
		use et_general;
	begin	
		if not exists (compose (work_directory, report_directory)) then
			put_line ("creating report directory ...");
			create_directory (compose (work_directory, report_directory));
		end if;
	end create_report_directory;

	procedure import_project is
	-- As a result of the import, a native project is created in the work_directory (ET/...).
		use et_schematic;
		use et_project.type_project_name;
		use et_import;
	begin
		-- Test if project name specified and if project base directory exists:
		if length (project_name_import) > 0 then

			-- If project name was provided with a trailing directory separator it must be removed.
			project_name_import := et_project.to_project_name (strip_directory_separator (et_project.to_string (project_name_import)));
			validate_project (project_name_import, et_import.cad_format);
		else
			put_line (message_error & "project name not specified !");
			raise constraint_error;
		end if;

		-- Test if cad format specified:
		if et_import.cad_format = UNKNOWN then
			put_line (message_error & "CAD format not specified !");
			raise constraint_error;
		end if;		

		-- CS not required for importing foreign projects. Update README.md. :
		-- read conventions file if specified. otherwise issue warning
		if conventions.type_conventions_file_name.length (conv_file_name_use) > 0 then
			conventions.read_conventions (conv_file_name_use, log_threshold => 0);
		else
			log (WARNING, "no conventions file specified !");
		end if;
		
		-- The import requires changing of directories. So we backup the current directory.
		-- After the import, we restore the directory.
		backup_projects_root_directory;

		log (text => "importing project " & et_project.to_string (project_name_import) & " ...", console => true);
		log (text => "CAD format " & to_string (et_import.cad_format));
				
		case et_import.cad_format is
			when et_import.KICAD_V4 | et_import.KICAD_V5 =>

				-- do the import
				et_kicad.import_design (project => project_name_import, log_threshold => 0);
				restore_projects_root_directory;

				-- convert to native project (with a default rig configuration file)
				log (text => et_string_processing.row_separator_single);
				log (text => "converting to " & et_general.system_name & " native project ...", console => true);
				log_indentation_up;
				et_kicad_to_native.to_native (log_threshold => 0);
				log_indentation_down;
				
			when others => -- CS
				raise constraint_error;
		end case;

		exception
			when event:
				others =>
					put_line (standard_output, message_error & "Read import report for warnings and error messages !"); -- CS: show path to report file
					raise;

	end import_project;

	procedure save_package_as is 
		use et_packages.type_package_model_file;
	begin
		if length (package_name_save_as) > 0 then
			pcb_rw.save_package (
				file_name 		=> package_name_save_as,
				packge			=> et_packages.type_packages.last_element (et_packages.packages),
				log_threshold	=> 0);
		end if;
	end;
	
	procedure process_commandline_arguments is
		use et_project.type_project_name;
		use scripting.type_script_name;
		use conventions.type_conventions_file_name;
		use et_packages.type_package_model_file;

		procedure read_configuration_file is begin
			if length (conv_file_name_use) > 0 then
				conventions.read_conventions (
					file_name		=> conv_file_name_use,
					log_threshold	=> 0);
			end if;
		end read_configuration_file;

		exit_code_script : scripting.type_exit_code;
		
	begin -- process_commandline_arguments
		-- The arguments are processed according to a certain priority.
		-- 1. create conventions file
		-- 2. import foreign project
		-- 3. open native project
		-- 3.1. execute script on native project

		-- If the operator wishes to create a conventions file it will be done.
		-- Other command line parameters are ignored:
		if length (conv_file_name_create) > 0 then
			conventions.make_default_conventions (conv_file_name_create, log_threshold => 0);
		else
			-- If operator wants to create a new project it will be created in the current directory:
			if length (project_name_create) > 0 then
				et_project.create_project_directory (
					project_name	=> project_name_create,
					project_path	=> et_project.to_project_path (""),
					log_threshold	=> 0);
				
			-- If operator wants to import a project it will be done here.
			elsif length (project_name_import) > 0 then
				read_configuration_file;
				import_project;

			-- Otherwise a native project will be opened:
			elsif length (project_name_open) > 0 then
				read_configuration_file;
				et_project.open_project (project_name_open, log_threshold => 0);

				-- If operator whishes to execute a script on the native project:
				if length (script_name) > 0 then
					exit_code_script := scripting.execute_script (script_name, log_threshold => 0);

					-- evaluate exit code
					case exit_code_script is
						when scripting.ERROR =>
							log (ERROR, " execution of script " & to_string (script_name) &
								" failed !", console => true);
							raise constraint_error;

						when scripting.WARNINGS =>
							log (WARNING, " execution of script " & to_string (script_name) &
								 " produced warnings !", console => true);

						when scripting.SUCCESSFUL =>
							log (text => "execution of script " & to_string (script_name) & " successful");
							
					end case;
				end if;
				
				-- optionally the project can be saved elsewhere
				if length (project_name_save_as) > 0 then
					et_project.save_project (project_name_save_as, log_threshold => 0);
				end if;

			elsif length (package_name_create) > 0 then
				pcb_rw.create_package (package_name_create, package_appearance, log_threshold => 0);

				-- optionally the package can be saved under a different name
				save_package_as;

			elsif length (package_name_import) > 0 then
				null; -- CS

			elsif length (package_name_open) > 0 then
				pcb_rw.read_package (package_name_open, log_threshold => 0);

				-- optionally the package can be saved under a different name
				save_package_as;
			end if;
			
		end if;
		
	end process_commandline_arguments;

	
begin -- main
	-- create a directory where imported projects live:
	create_work_directory;

	-- create inside the word directory another directory for reports and log messages:
	create_report_directory;

	create_report;
	
	get_commandline_arguments;

	process_commandline_arguments;
	

	close_report;

	exception
		when event: others =>
			log_indentation_reset;
			log (text => ada.exceptions.exception_information (event), console => true);
			close_report;
			
			put_line ("Read log file " & log_file_name & " for details !");
			set_exit_status (failure);

end et;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
