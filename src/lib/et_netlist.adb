------------------------------------------------------------------------------
--                                                                          --
--                    SYSTEM ET NETLIST DECLARATIONS                        --
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

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;

with ada.text_io;				use ada.text_io;

with ada.containers;            use ada.containers;
with ada.containers.indefinite_ordered_maps;


with ada.directories;

with et_general;
with et_libraries;
with et_schematic;
with et_string_processing;		use et_string_processing;
with et_coordinates;
with et_geometry;
with et_import;
with et_export;

package body et_netlist is

	procedure first_module is
	-- Resets the module_cursor to the first module of the rig.
	begin
		module_cursor := rig.first;
		-- CS: exception handler in case given module does not exist
	end first_module;

	function first_net return type_netlist.cursor is
	-- Returns a cursor to the first net of the current module (indicated by module_cursor).
		cursor : type_netlist.cursor;
	
		procedure set (
			module	: in et_coordinates.type_submodule_name.bounded_string;
			netlist	: in type_netlist.map) is
		begin
			cursor := netlist.first;
		end set;

	begin -- first_net
		type_rig_netlists.query_element (
			position => module_cursor,
			process => set'access);

		return cursor;
	end first_net;

	function net_count return count_type is
	-- Returns the number of nets of the current module as string.
		count : count_type := 0;
	
		procedure get (
			module	: in et_coordinates.type_submodule_name.bounded_string;
			netlist	: in type_netlist.map) is
		begin
			count := length (netlist);
		end get;

	begin
		type_rig_netlists.query_element (
			position => module_cursor,
			process => get'access);

		return count;
	end net_count;
	
	function first_port (net_cursor : in type_netlist.cursor) return et_schematic.type_ports.cursor is
	-- Returns a cursor to the first port of the given net in the current module (indicated by module_cursor).
		cursor : et_schematic.type_ports.cursor;
	
		procedure set (
			net		: in et_schematic.type_net_name.bounded_string;
			ports	: in et_schematic.type_ports.set) is
		begin
			cursor := ports.first;
		end set;

	begin -- first_port
		type_netlist.query_element (
			position => net_cursor,
			process => set'access);

		return cursor;
	end first_port;

	function port_count (net_cursor : in type_netlist.cursor) return count_type is
	-- Returns the number of ports of the given net of the current module.
		count : count_type := 0;

		procedure get (
			net		: in et_schematic.type_net_name.bounded_string;
			ports	: in et_schematic.type_ports.set) is
		begin
			count := length (ports);
		end get;
					  
	begin -- port_count
		type_netlist.query_element (
			position => net_cursor,
			process => get'access);
		return count;
	end port_count;
	
	procedure make_netlists is
	-- Builids the netlists of all modules in the rig.
	-- Netlists are to be exported in individual project directories in the work directory of ET.
	-- These project directories have the same name as the module indicated by module_cursor.

		use et_schematic;
		use et_schematic.type_rig;
		use et_export;
		use ada.directories;

		-- Here the portlists of the current module are stored by function build_portlists:
		portlists : type_portlists.map;

		function make_netlist return type_netlist.map is
		-- Generates the netlist of the current module (indicated by module_cursor).
		-- The portlists provide the port coordinates. 
		-- The module provides the nets.
		-- With this information we make the netlist of the current module.
		
			net_cursor_netlist : type_netlist.cursor; -- points to the current net within the netlist being built
			net_inserted : boolean; -- indicates whether a net has been inserted into the netlist
			
			net_cursor_module : type_nets.cursor := first_net; 
			-- points to the net being read from the module (see type_module in et_schematic.ads)
		
			netlist : type_netlist.map; -- the netlist being built. to be returned finally.
			use type_nets;
			
			segment_cursor : type_net_segments.cursor; -- points to the net segment being read from the module
			segment : type_net_segment; -- the actual net segment read from the module
			use type_net_segments;

			component_cursor : type_portlists.cursor; -- point to the component within the portlists
			use type_portlists;

			port_cursor : type_base_ports.cursor; -- points to the port being read from a component within the portlists
			port : type_port_base; -- the actual port read from the portlists
			use type_base_ports;

			use et_string_processing;

			procedure add_port (
			-- Adds the given port and the component reference to the net within the netlist being built.
				reference : in et_libraries.type_component_reference;
				port : in type_port_base) is

				procedure add (
					net_name : in type_net_name.bounded_string;
					ports : in out type_ports.set) is
				begin
					--ports.insert (port with reference); -- CS: does not compile with gnat 4.8.5 . may work with later versions
					ports.insert (new_item => (port with reference));
				end add;
				
			begin -- add_port
				netlist.update_element (
					position => net_cursor_netlist,
					process => add'access);
			end add_port;
			
		begin -- make_netlist (note singluar !)
			log (text => "building module netlist ...", level => 1);

			-- HOW DOES IT WORK ?
			-- The outer loop fetches one net after another from the module (see type_module in et_schematic.ads).
			-- The net name is used as primary key for the net being inserted into the netlist.
			-- The loop in the next level fetches the net segments from the net.

			-- The loop in the next level fetches components from the portlists.
			-- The loop in the next level fetches the ports of the component within the portlists.

			-- Then we test if a port sits on the current segment. When positive, the port is added to 
			-- the net within the netlist.
			
			-- LOOP IN NETS OF MODULE (outer loop)
			while net_cursor_module /= type_nets.no_element loop

				-- get the net name via net_cursor
				log_indentation_up;
				log (text => "net " & et_schematic.type_net_name.to_string (key (net_cursor_module)), level => 2);
				
				-- Insert net in netlist with the net name as primary key:
				netlist.insert (
					key => key (net_cursor_module), -- net name like "MCU_CLOCK"
					new_item => type_ports.empty_set, -- for the moment an empty portlist
					inserted => net_inserted, -- CS: check status ?
					position => net_cursor_netlist);

				-- loop in segments of net
				segment_cursor := first_segment (net_cursor_module);
				while segment_cursor /= type_net_segments.no_element loop
					segment := element (segment_cursor);
					log_indentation_up;
					log (text => "probing segment " & to_string (segment), level => 2);

					-- LOOP IN PORTLISTS
					component_cursor := first (portlists);
					while component_cursor /= type_portlists.no_element loop
						log ("probing component " & et_schematic.to_string (key (component_cursor)), level => 3);
						
						port_cursor := first_port (component_cursor);
						while port_cursor /= type_base_ports.no_element loop

							-- CS: skip already processed ports to improve performance
							
							port := element (port_cursor); -- in portlist of component
							log ("probing port " & et_coordinates.to_string (port.coordinates), level => 3);

							-- test if port sits on segment
							if port_sits_on_segment (port, segment) then
								log_indentation_up;
								log ("comp " & et_schematic.to_string (key (component_cursor)), level => 2);
								log ("port " & et_coordinates.to_string (port.coordinates), level => 2);
								log_indentation_down;

								-- add port to current net in netlist
								add_port (
									reference => key (component_cursor),
									port => port);

							end if;
							
							next (port_cursor);
						end loop;

						next (component_cursor);
					end loop;

					log_indentation_down;
					next (segment_cursor);
				end loop;

				log_indentation_down;
				next (net_cursor_module);
			end loop;

			log (text => "processed nets" & count_type'image (length (netlist)), level => 2);
			return netlist;
		end make_netlist;
		
	begin -- make_netlists (note plural !)
		log (text => "building rig netlists ...", level => 1);

		-- We start with the first module of the rig.
		et_schematic.first_module;

		-- Process one rig module after another.
		-- module_cursor points to the module in the rig.
		while et_schematic.module_cursor /= type_rig.no_element loop
			log_indentation_up;
			log (text => "module " & to_string (key (et_schematic.module_cursor)), level => 1);
			create_project_directory (to_string (key (et_schematic.module_cursor)));

			-- Generate the portlists of the module indicated by module_cursor.
			portlists := build_portlists;

			-- Insert the module in rig_netlists with the netlist built by make_netlist:
			rig.insert (
				key => key (et_schematic.module_cursor),
				new_item => make_netlist);

			next (et_schematic.module_cursor);
		end loop;

		log_indentation_down;
	end make_netlists;


	procedure write_netlists is
	-- Exports/Writes the netlists of the rig in separate files.
	-- Call this procedure after executing procedure make_netlist !
		use type_rig_netlists;
		use ada.directories;
		use et_general;
	
		netlist_handle : ada.text_io.file_type;
		netlist_file_name : type_netlist_file_name.bounded_string;
	
		net_cursor	: type_netlist.cursor; -- points to the net being exported
		net_name	: et_schematic.type_net_name.bounded_string;
		port_cursor	: et_schematic.type_ports.cursor; -- point to the port being exported

-- 		component	: et_libraries.type_component_reference;
-- 		port		: et_libraries.type_pin_name.bounded_string;
-- 		pin			: et_libraries.type_port_name.bounded_string;
		port		: et_schematic.type_port;


	begin -- write_netlists
		first_module;
		
		log (text => "writing rig netlists ...", level => 1);
		while module_cursor /= type_rig_netlists.no_element loop
			log_indentation_up;
			log (text => "module " & to_string (key (module_cursor)), level => 1);

			-- compose the netlist file name and its path like "../ET/motor_driver/motor_driver.net"
			netlist_file_name := type_netlist_file_name.to_bounded_string (
				compose (
					containing_directory => compose (work_directory, to_string (key (module_cursor))),
					name => to_string (key (module_cursor)),
					extension => "net"));

			-- create the netlist (which inevitably overwrites the previous file)
			log_indentation_up;
			log (text => "creating netlist file " & type_netlist_file_name.to_string (netlist_file_name), level => 1);
			create (
				file => netlist_handle,
				mode => out_file, 
				name => type_netlist_file_name.to_string (netlist_file_name));

			-- CS: write a nice header with legend in netlist
			
			-- export net
			log_indentation_up;
			log (text => "exporting" & count_type'image (net_count) & " nets ...", level => 1);
			net_cursor := first_net;
			while net_cursor /= type_netlist.no_element loop
				net_name := key (net_cursor);

				-- write net name in netlist
				log_indentation_up;
				log (text => "net " & et_schematic.type_net_name.to_string (net_name), level => 1);
				new_line (netlist_handle);
				put_line (netlist_handle, et_schematic.type_net_name.to_string (net_name));

				-- export port
				log_indentation_up;
				log (text => "exporting" & count_type'image (port_count (net_cursor)) & " ports ...", level => 1);
				port_cursor := first_port (net_cursor);
				while port_cursor /= et_schematic.type_ports.no_element loop
					port := element (port_cursor);

					-- write reference, port, pin in netlist (all in a single line)
					put_line (netlist_handle, 
						et_schematic.reference (port) & " "
						& et_schematic.port (port) & " "
						& et_schematic.pin (port) & " "
						);
					
					next (port_cursor);
				end loop;
				log_indentation_down;
				
				log_indentation_down;

				next (net_cursor);
			end loop;
			
			close (netlist_handle);

			log_indentation_down;
			log_indentation_down;
			log_indentation_down;

			
			next (module_cursor);
		end loop;
		
	end write_netlists;
		
end et_netlist;

-- Soli Deo Gloria
