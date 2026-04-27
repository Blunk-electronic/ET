------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             NETLISTS                                     --
--                                                                          --
--                              B o d y                                     --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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
--   To Do: 
--
--


with ada.text_io;						use ada.text_io;
with ada.strings;

with ada.directories;
with gnat.directory_operations;

with et_export;
with et_system_info;
with et_time;
with et_netlist_name;
with et_string_processing;
with et_device_name;
with et_port_direction;			
with et_port_names;
with et_terminal_name;
with et_module_instance;

-- with et_netchangers;
with et_netchangers.schematic;


package body et_netlist_cat_1 is



	procedure add_net_to_netlist (
		netlist		: in out pac_netlist_cat_1.map;
		name		: in pac_net_name.bounded_string;
		devices		: in pac_device_ports_extended.set;
		submodules	: in pac_submodule_ports_extended.set;
		netchangers	: in pac_netchanger_ports.set)
	is begin
		netlist.insert (
			key			=> name,
			new_item	=> (devices, submodules, netchangers));

	end;





	
	procedure write_netlist (
		module_cursor	: in pac_generic_modules.cursor;
		variant			: in pac_assembly_variant_name.bounded_string;
		netlist			: in pac_netlist_cat_1.map;
		log_threshold	: in type_log_level)
	is
		use et_netlist_name;
		file_name	: pac_netlist_file_name.bounded_string;
		file_handle	: ada.text_io.file_type;


		procedure set_file_name is 
			use ada.directories;
			use gnat.directory_operations;
			use pac_assembly_variant_name;
			use et_export;

			module_name : constant pac_module_name.bounded_string := 
				get_module_name (module_cursor);
			
		begin
			if is_default (variant) then
				file_name := to_file_name (
					compose (
						containing_directory	=> directory_export & dir_separator & directory_cam &
													dir_separator & directory_netlists,

						name					=> to_string (module_name),
						extension				=> extension_netlist));

			else
				file_name := to_file_name (
					compose (
						containing_directory	=> directory_export & dir_separator & directory_cam &
													dir_separator & directory_netlists,

						name					=> to_string (module_name) & "_" & 
													to_variant (variant),
						
						extension				=> extension_netlist));
			end if;
		end set_file_name;	



		-- Writes a nice header in the netlist file:
		procedure write_header is 
			use et_system_info;
			use et_time;
			use et_string_processing;
		begin
			put_line (file_handle, comment_mark & " " & system_name & " " & version & " netlist");
			put_line (file_handle, comment_mark & " " & get_date);
			put_line (file_handle, comment_mark & " module " & to_string (module_cursor));
			put_line (file_handle, comment_mark & " " & row_separator_double);
			-- CS put_line (file_handle, comment_mark & " net count total" & count_type'image (child_count (netlist_cursor)));
			-- CS: use dedicated subtype for net count instead of count_type
			-- CS: statistics about pin count ?
			
			put_line (file_handle, comment_mark);
			put_line (file_handle, comment_mark & " legend:");
			put_line (file_handle, comment_mark & "  net_name");
			put_line (file_handle, comment_mark & "  device port direction terminal/pin/pad");
			put_line (file_handle, comment_mark);
			-- put_line (file_handle, comment_mark & "  Names of secondary nets are comments.");
			put_line (file_handle, comment_mark & " " & row_separator_single);
		end write_header;



		-- Writes a nice footer in the netlist file:
		procedure write_footer is 
			use et_string_processing;
		begin
			new_line (file_handle);
			put_line (file_handle, comment_mark & " " & row_separator_double);
			put_line (file_handle, comment_mark & " end of list");
		end write_footer;



		-- Writes the nets and connected devices, netchangers,
		-- and submodules in the netlist file:
		procedure write_nets is

			use pac_netlist_cat_1;
			net_cursor : pac_netlist_cat_1.cursor := netlist.first;

			
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in type_net_ports_cat_1)
			is			
				use ada.strings;
				separator : constant character := space;

			
				-- This procedure writes the ports of
				-- the devices which are connected with the
				-- candidate net:
				procedure write_devices is 

					use pac_device_ports_extended;
					device_cursor : pac_device_ports_extended.cursor :=
						net.devices.first;

				
					procedure query_device (
						device : in type_device_port_extended)
					is	
						use et_device_name;
						use et_port_direction;
						use et_port_names;
						use et_terminal_name;
					begin
						put_line (file_handle, -- IC1 CE input H5
							to_string (device.device) & separator
							& to_string (device.port) & separator
							& to_string (device.direction) & separator
							& to_string (device.terminal));
						-- CS characteristics
					end query_device;
				
				
				begin
					-- Iterate through the devices of the candidate net:
					while has_element (device_cursor) loop				
						query_element (device_cursor, query_device'access);
						next (device_cursor);
					end loop;
				end write_devices;
				
				
				
				-- This procedure writes the ports of
				-- the submodules which are connected with the
				-- candidate net:
				procedure write_submodules is
					use pac_submodule_ports_extended;
					submodule_cursor : pac_submodule_ports_extended.cursor :=
						net.submodules.first;
						
						
					procedure query_submodule (
						submodule : type_submodule_port_extended)
					is 
						use et_module_instance;
					begin
						put_line (file_handle, -- OSC1 RF_OUT
							to_string (submodule.submodule) & separator
							& to_string (submodule.port));
							-- CS direction, characteristics
					end query_submodule;
					

				begin
					-- Iterate through the submodules of the candidate net:
					while has_element (submodule_cursor) loop
						query_element (submodule_cursor, query_submodule'access);
						next (submodule_cursor);
					end loop;
				end write_submodules;
				
				

				
				-- This procedure writes the ports of
				-- the netchangers which are connected with the
				-- candidate net:
				procedure write_netchangers is
					use pac_netchanger_ports;
					netchanger_cursor : pac_netchanger_ports.cursor :=
						net.netchangers.first;
						
						
					procedure query_netchanger (
						netchanger : type_port_netchanger)
					is 
						use et_netchangers;
						use et_netchangers.schematic;
					begin
						put_line (file_handle, -- N14 master/slave
							get_netchanger_name (netchanger.index) & separator
							& to_string (netchanger.port));
					end query_netchanger;
					

				begin
					-- Iterate through the netchangers of the candidate net:
					while has_element (netchanger_cursor) loop
						query_element (netchanger_cursor, query_netchanger'access);
						next (netchanger_cursor);
					end loop;
				end write_netchangers;

				
				
			begin
				log (text => "net " & to_string (net_name),
					level => log_threshold + 1);

				new_line (file_handle);
				-- put_line (file_handle, comment_mark & " -------");
				put_line (file_handle, to_string (net_name));

				write_devices;				
				write_submodules;				
				write_netchangers;	
			end query_net;

			
		begin
			while has_element (net_cursor) loop
				query_element (net_cursor, query_net'access);
				next (net_cursor);
			end loop;
		end write_nets;
		
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " assembly variant " & to_variant (variant)
			& " write netlist",
			-- CS full file name and directory
			level => log_threshold);
			
		log_indentation_up;
		
		set_file_name;

		create (
			file => file_handle,
			mode => out_file, 
			name => to_string (file_name));

		write_header;
		write_nets;
		write_footer;
		
		close (file_handle);

		log_indentation_down;

		
		exception
			when event: others =>
				if is_open (file_handle) then
					close (file_handle);
				end if;
				
				log_indentation_reset;
				-- log (text => ada.exceptions.exception_information (event), console => true);
				-- raise;

		
	end write_netlist;
	
	
	
	
end et_netlist_cat_1;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
