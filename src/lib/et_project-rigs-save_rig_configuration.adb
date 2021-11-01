------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                    PROJECT SAVE RIG CONFIGURATION                        --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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

separate (et_project.rigs)

procedure save_rig_configuration (
	rig_cursor		: in pac_rigs.cursor;
	log_threshold 	: in type_log_level) 
is

	use pac_rigs;
	use ada.directories;
	use type_module_instances;
	use type_module_connectors;

	-- For the final full file name like /home/user/et_projects/blood_sample_analyzer.conf
	file_name : pac_file_name.bounded_string;

	-- backup the previous output
	previous_output : ada.text_io.file_type renames current_output;
	
	file_handle : ada.text_io.file_type;

	procedure query_instance (instance_cursor : in type_module_instances.cursor) is
	begin
		section_mark (section_module, HEADER);			
		write (keyword => keyword_generic_name, parameters => to_string (element (instance_cursor).generic_name));
		write (keyword => keyword_instance_name, parameters => et_general.to_string (key (instance_cursor)));
		section_mark (section_module, FOOTER);
	end;

	procedure query_connections (connection_cursor : in type_module_connectors.cursor) is
		con : type_connector := element (connection_cursor);
	begin
		section_mark (section_connector, HEADER);
		write (keyword => keyword_instance_A, parameters => to_string (con.instance_A));
		write (keyword => keyword_purpose_A, wrap => true, parameters => et_devices.to_string (con.purpose_A));
		new_line;
		write (keyword => keyword_instance_B, parameters => to_string (con.instance_B));
		write (keyword => keyword_purpose_B, wrap => true, parameters => et_devices.to_string (con.purpose_B));

		-- CS: net comparator, warnings
		
		section_mark (section_connector, FOOTER);
	end;
	
begin -- save_rig_configuration
	log (text => "saving rig configuration ...", level => log_threshold);
	reset_tab_depth;
	log_indentation_up;

	-- compose the full file name
	file_name := pac_file_name.to_bounded_string (compose (
		name 		=> to_string (key (rig_cursor)), -- fully_equipped
		extension 	=> file_extension)); -- conf

	-- create the file
	create (
		file => file_handle,
		mode => out_file, 
		name => to_string (file_name));
	
	set_output (file_handle);
	write_rig_configuration_header;		

	-- section module instances
	section_mark (section_module_instances, HEADER);
	iterate (element (rig_cursor).module_instances, query_instance'access);
	-- CS In the future, write other things here that characterize the instance.
	section_mark (section_module_instances, FOOTER);

	-- section connectors
	new_line;
	section_mark (section_module_connections, HEADER);
	iterate (element (rig_cursor).connections, query_connections'access);
	-- CS In the future, write other things here that characterize the board to board connection
	section_mark (section_module_connections, FOOTER);

	-- close the file
	write_rig_configuration_footer;
	set_output (previous_output);
	close (file_handle);
	
	log_indentation_down;

	exception when event:
		others => 
			log (text => ada.exceptions.exception_message (event), console => true);
			close (file_handle);
			set_output (previous_output);
			raise;

end save_rig_configuration;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
