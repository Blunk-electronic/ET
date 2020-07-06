------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      PROJECT CONFIGURATON SAVE                           --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

separate (et_project.configuration)
	
procedure save_configuration (
	project_name 	: in type_project_name.bounded_string; -- blood_sample_analyzer
	log_threshold 	: in et_string_processing.type_log_level) 
is
	use et_string_processing;
	use ada.directories;

	-- compose the name of the project file to create like blood_sample_analyzer.prj
	file_name : constant string := 
		compose (to_string (project_name), to_string (project_name), file_extension);

	-- backup the previous output destination
	previous_output : ada.text_io.file_type renames current_output;
	
	file_handle : ada.text_io.file_type;

	procedure write_rules is 
		use et_conventions;
	begin
		section_mark (section_rules, HEADER);

		if conventions_specified then
			write (keyword => keyword_conventions, parameters => to_string (project.rules.conventions));
		end if;
		
		section_mark (section_rules, FOOTER);
	end write_rules;
	
begin -- save_configuration
	log (text => "saving project configuration file " & enclose_in_quotes (file_name) & " ...",
		 level => log_threshold, console => true);
	log_indentation_up;
	
	-- create the file
	create (
		file => file_handle,
		mode => out_file, 
		name => file_name);
	
	set_output (file_handle);
	write_configuration_header;

	write_rules;

	-- CS write_environment_variables

	-- CS write other stuff

	write_configuration_footer;	
	set_output (previous_output);
	
	close (file_handle);
	
	log_indentation_down;

	exception when event:
		others => 
			log (text => ada.exceptions.exception_message (event), console => true);
			close (file_handle);
			raise;

end save_configuration;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
