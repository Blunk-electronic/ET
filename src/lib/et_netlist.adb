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

	procedure dummy is begin null; end dummy;

-- 	function reference (port : in type_port) return string is
-- 	-- Returns the component reference of the given port.	
-- 	begin
-- 		return et_schematic.to_string (port.reference);
-- 	end reference;
-- 
-- 	function port (port : in type_port) return string is
-- 	-- Returns the port name of the given port.
-- 	begin
-- 		return et_libraries.to_string (port.port);
-- 	end port;
-- 
-- 	function pin (port : in type_port) return string is
-- 	-- Returns the pin name of the given port.
-- 	begin
-- 		return et_libraries.to_string (port.pin);
-- 	end pin;
-- 
-- 	function appearance (port : in type_port) return et_libraries.type_component_appearance is
-- 	-- Returns the appearance of the given port.
-- 	begin
-- 		return port.appearance;
-- 	end appearance;
-- 	
-- 	function compare_ports (left, right : in type_port) return boolean is
-- 	-- Returns true if left comes before right. Compares by component name and pin name.
-- 	-- If left equals right, the return is false.	
-- 	-- CS: needs verification !
-- 		result : boolean := false;
-- 		use et_libraries;
-- 		use et_schematic;
-- 	begin
-- 		-- First we compare the component reference.
-- 		-- Examples: C56 comes before R4, LED5 comes before LED7
-- 		if compare_reference (left.reference, right.reference) then
-- 			result := true;
-- 
-- 		-- If equal references, compare pin names
-- 		elsif type_pin_name.">" (left.pin, right.pin) then
-- 			result := true;
-- 
-- 		-- If equal pin names, compare port names -- CS: should never happen. raise alarm ?
-- 		elsif type_port_name.">" (left.port, right.port) then
-- 			result := true;
-- 			
-- 		else
-- 			result := false;
-- 		end if;
-- 
-- 		-- in case of equivalence of left and right, we return false (default)
-- 		return result;
-- 	end compare_ports;
-- 
-- 	function appearance (port : in type_ports.cursor) return et_libraries.type_component_appearance is
-- 	-- Returns the appearance of the given port.
-- 	begin
-- 		return (element (port).appearance);
-- 	end appearance;


-- 	function first_port (component_cursor : in type_portlists.cursor) return type_base_ports.cursor is
-- 	-- Returns a cursor pointing to the first port of a component in the portlists.
-- 		port_cursor : type_base_ports.cursor;
-- 	
-- 		procedure set_cursor (
-- 			name : in et_libraries.type_component_reference;
-- 			ports : in type_base_ports.list) is
-- 		begin
-- 			port_cursor := first (ports);
-- 		end set_cursor;
-- 		begin -- first_port
-- 		type_portlists.query_element (
-- 			position => component_cursor,
-- 			process => set_cursor'access);
-- 
-- 		return port_cursor;
-- 	end first_port;

	
	
-- 	procedure first_module is
-- 	-- Resets the module_cursor to the first module of the rig.
-- 	begin
-- 		module_cursor := rig.first;
-- 		-- CS: exception handler in case given module does not exist
-- 	end first_module;

-- 	function first_net return type_netlist.cursor is
-- 	-- Returns a cursor to the first net of the current module (indicated by module_cursor).
-- 		cursor : type_netlist.cursor;
-- 	
-- 		procedure set (
-- 			module	: in et_coordinates.type_submodule_name.bounded_string;
-- 			netlist	: in type_netlist.map) is
-- 		begin
-- 			cursor := netlist.first;
-- 		end set;
-- 
-- 	begin -- first_net
-- 		type_rig_netlists.query_element (
-- 			position => module_cursor,
-- 			process => set'access);
-- 
-- 		return cursor;
-- 	end first_net;

-- 	function net_count return count_type is
-- 	-- Returns the number of nets of the current module as string.
-- 		count : count_type := 0;
-- 	
-- 		procedure get (
-- 			module	: in et_coordinates.type_submodule_name.bounded_string;
-- 			netlist	: in type_netlist.map) is
-- 		begin
-- 			count := length (netlist);
-- 		end get;
-- 
-- 	begin
-- 		type_rig_netlists.query_element (
-- 			position => module_cursor,
-- 			process => get'access);
-- 
-- 		return count;
-- 	end net_count;
	
-- 	function first_port (net_cursor : in type_netlist.cursor) return type_ports.cursor is
-- 	-- Returns a cursor to the first port of the given net in the current module (indicated by module_cursor).
-- 		cursor : type_ports.cursor;
-- 	
-- 		procedure set (
-- 			net		: in et_schematic.type_net_name.bounded_string;
-- 			ports	: in type_ports.set) is
-- 		begin
-- 			cursor := ports.first;
-- 		end set;
-- 
-- 	begin -- first_port
-- 		type_netlist.query_element (
-- 			position => net_cursor,
-- 			process => set'access);
-- 
-- 		return cursor;
-- 	end first_port;
-- 
-- 	function port_count (net_cursor : in type_netlist.cursor) return count_type is
-- 	-- Returns the number of ports of the given net of the current module.
-- 		count : count_type := 0;
-- 
-- 		procedure get (
-- 			net		: in et_schematic.type_net_name.bounded_string;
-- 			ports	: in type_ports.set) is
-- 		begin
-- 			count := length (ports);
-- 		end get;
-- 					  
-- 	begin -- port_count
-- 		type_netlist.query_element (
-- 			position => net_cursor,
-- 			process => get'access);
-- 		return count;
-- 	end port_count;
-- 
-- 	function component_ports_total return count_type is
-- 	-- Returns the total number of component ports in the current module.
-- 	-- The return does not include so called "power_flags".
-- 		n : count_type := 0;
-- 
-- 		procedure get (
-- 			module	: in et_coordinates.type_submodule_name.bounded_string;
-- 			netlist	: in type_netlist.map) is
-- 
-- 			net : type_netlist.cursor;
-- 		begin
-- 			net := netlist.first;
-- 
-- 			-- loop trough the nets and summ up the number of ports.
-- 			while net /= type_netlist.no_element loop
-- 				n := n + port_count (net);
-- 				next (net);
-- 			end loop;
-- 		end get;
-- 
-- 	begin -- component_ports_total
-- 		type_rig_netlists.query_element (
-- 			position => module_cursor,
-- 			process => get'access);
-- 
-- 		return n;
-- 
-- 	end component_ports_total;
-- 	
-- 	procedure set_module (module_name : in et_coordinates.type_submodule_name.bounded_string) is
-- 	-- Sets the active module. Leaves module_cursor pointing to the module.
-- 	begin
-- 		module_cursor := rig.find (module_name);
-- 	end set_module;
-- 	
-- 	
-- 
-- 	procedure write_netlists is
-- 	-- Exports/Writes the netlists of the rig in separate files.
-- 	-- Call this procedure after executing procedure make_netlist !
-- 		use type_rig_netlists;
-- 		use ada.directories;
-- 		use et_general;
-- 		use et_libraries;
-- 		
-- 		netlist_handle : ada.text_io.file_type;
-- 		netlist_file_name : type_netlist_file_name.bounded_string;
-- 	
-- 		net_cursor	: type_netlist.cursor; -- points to the net being exported
-- 		net_name	: et_schematic.type_net_name.bounded_string;
-- 		port_cursor	: type_ports.cursor; -- point to the port being exported
-- 		port		: type_port;
-- 
-- 	begin -- write_netlists
-- 		first_module;
-- 		
-- 		log (text => "writing rig netlists ...", level => 1);
-- 		while module_cursor /= type_rig_netlists.no_element loop
-- 			log_indentation_up;
-- 			log (text => "module " & to_string (key (module_cursor)), level => 1);
-- 
-- 			-- compose the netlist file name and its path like "../ET/motor_driver/CAM/motor_driver.net"
-- 			netlist_file_name := type_netlist_file_name.to_bounded_string 
-- 				(
-- 				compose (
-- 					containing_directory => compose 
-- 						(
-- 						containing_directory => compose (work_directory, to_string (key (module_cursor))),
-- 						name => et_export.directory_cam
-- 						),
-- 					name => to_string (key (module_cursor)),
-- 					extension => extension_netlist)
-- 				);
-- 
-- 			-- create the netlist (which inevitably and intentionally overwrites the previous file)
-- 			log_indentation_up;
-- 			log (text => "creating netlist file " & type_netlist_file_name.to_string (netlist_file_name), level => 1);
-- 			create (
-- 				file => netlist_handle,
-- 				mode => out_file, 
-- 				name => type_netlist_file_name.to_string (netlist_file_name));
-- 
-- 			put_line (netlist_handle, comment_mark & " " & system_name & " netlist");
-- 			put_line (netlist_handle, comment_mark & " date " & string (date_now));
-- 			put_line (netlist_handle, comment_mark & " module " & to_string (key (module_cursor)));
-- 			put_line (netlist_handle, comment_mark & " " & row_separator_double);
-- 			-- CS: statistics about net count and pin count ?
-- 			put_line (netlist_handle, comment_mark & " legend:");
-- 			put_line (netlist_handle, comment_mark & "  net name");
-- 			put_line (netlist_handle, comment_mark & "  component port pin/pad direction");
-- 			put_line (netlist_handle, comment_mark & " " & row_separator_single);
-- 			
-- 			-- export net
-- 			log_indentation_up;
-- 			log (text => "exporting" & count_type'image (net_count) & " nets ...", level => 1);
-- 			net_cursor := first_net;
-- 			while net_cursor /= type_netlist.no_element loop
-- 				net_name := key (net_cursor);
-- 
-- 				-- write net name in netlist
-- 				log_indentation_up;
-- 				log (text => "net " & to_string (net_name), level => 1);
-- 
-- 				-- Check if net has no dedicated name (a net name like N$3 is anonymous):
-- 				if et_schematic.anonymous (net_name) then
-- 					log (message_warning & "net " & to_string (net_name) & " has no dedicated name !");
-- 				end if;
-- 				
-- 				new_line (netlist_handle);
-- 				put_line (netlist_handle, to_string (net_name));
-- 
-- 				-- export port
-- 				log_indentation_up;
-- 				log (text => "exporting" & count_type'image (port_count (net_cursor)) & " ports ...", level => 1);
-- 				-- CS: warning if net has only one pin
-- 
-- 				-- CS: perfom an ERC on the current net in a separate loop (similar to the one below)
-- 				
-- 				port_cursor := first_port (net_cursor);
-- 				while port_cursor /= type_ports.no_element loop
-- 
-- 					-- we export only ports of real components
-- 					if appearance (port_cursor) = et_libraries.sch_pcb then
-- 
-- 						port := element (port_cursor);
-- 
-- 						-- write reference, port, pin in netlist (all in a single line)
-- 						-- CS: use port_cursor instead of a variable "port"
-- 						put_line (netlist_handle, 
-- 							reference (port) & " "
-- 							& et_netlist.port (port) & " "
-- 							& et_netlist.pin (port) & " "
-- 							);
-- 
-- 					end if;
-- 						
-- 					next (port_cursor);
-- 				end loop;
-- 				log_indentation_down;
-- 				
-- 				log_indentation_down;
-- 
-- 				next (net_cursor);
-- 			end loop;
-- 
-- 			new_line (netlist_handle);
-- 			put_line (netlist_handle, comment_mark & " " & row_separator_double);
-- 			put_line (netlist_handle, comment_mark & " end of list");
-- 			
-- 			close (netlist_handle);
-- 
-- 			log_indentation_down;
-- 			log_indentation_down;
-- 			log_indentation_down;
-- 
-- 			
-- 			next (module_cursor);
-- 		end loop;
-- 		
-- 	end write_netlists;
		
end et_netlist;

-- Soli Deo Gloria
