------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              NETLISTS                                    --
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
--   ToDo: 

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.text_io;				use ada.text_io;
with ada.strings; 				use ada.strings;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded;       use ada.strings.bounded;
with ada.containers;            use ada.containers;
with ada.containers.vectors;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;
with ada.directories;
with ada.exceptions;

-- with et_general;				use et_general;
-- 
-- with et_coordinates;
-- with et_libraries;
-- with assembly_variants;
with et_string_processing;		use et_string_processing;
with et_csv;					use et_csv;
-- with et_pcb_coordinates;
with submodules;
-- with numbering;

package body netlists is
	
	function to_string (name : in type_file_name.bounded_string) return string is begin
		return type_file_name.to_string (name);
	end;
	
	function to_file_name (name : in string) return type_file_name.bounded_string is begin
		return type_file_name.to_bounded_string (name);
	end;

	function "<" (left, right : in type_port) return boolean is
		use et_libraries;
		use type_port_name;
		result : boolean := false;
	begin
		if left.device < right.device then
			result := true;
			
		elsif left.device = right.device then

			if left.port < right.port then
				result := true;
			else
				result := false;
			end if;
			
		else
			result := false;
		end if;
		
		return result;
	end;

	function to_prefix (instance : in type_module_instance_name.bounded_string) -- OSC1
	-- Converts an instance name to a net prefix with a trailing level separator.
		return et_general.type_net_name.bounded_string is
	begin
		return to_net_name (to_string (instance) & level_separator);
	end;

	function "<" (left, right : in type_net_name) return boolean is
		result : boolean := false;
		use et_general.type_net_name;
	begin
		if left.prefix < right.prefix then
			result := true;

		elsif left.prefix = right.prefix then

			if left.base_name < right.base_name then
				result := true;
			else
				result := false;
			end if;

		else
			result := false;
		end if;

		return result;
	end;

	function port_count (net_cursor : in type_nets.cursor)
		return type_port_count is
	-- Returns the number of netchanger and submodule ports in the given net.

		port_count : type_port_count; -- to be returned

		procedure query_ports (
			net_name	: in type_net_name;
			net			: in type_net) is
			use et_schematic.type_ports_submodule;
			use et_schematic.type_ports_netchanger;

			procedure count_netchanger_ports (cursor : in et_schematic.type_ports_netchanger.cursor) is
				use submodules;
			begin
				case element (cursor).port is
					when MASTER => port_count.netchangers.masters := port_count.netchangers.masters + 1;
					when SLAVE => port_count.netchangers.slaves := port_count.netchangers.slaves + 1;
				end case;
			end count_netchanger_ports;
			
		begin -- query_ports
			-- The number of submodule ports is just the length of the 
			-- list of submodule ports:
			port_count.submodules := type_submodule_count (length (net.submodules));

			-- The number of master or slave ports of netchangers
			-- requires iterating and dividing them:
			iterate (net.netchangers, count_netchanger_ports'access);

			port_count.netchangers.total := port_count.netchangers.masters +
											port_count.netchangers.slaves;
			
		end query_ports;
			
	begin -- port_count
		type_nets.query_element (
			position	=> net_cursor,
			process		=> query_ports'access);
		
		return port_count;
	end port_count;

	function net_on_netchanger (
	-- Returns a cursor to the net connected with the given netchanger
	-- port opposide to the given port.
	-- If the given port is a master, then the net connected with the
	-- slave is returned (and vice versa).
	-- If the netchanger is not connected then the return is no_element.
		module_cursor	: in type_modules.cursor;
		port			: in et_schematic.type_port_netchanger)
		return type_nets.cursor is

		net_cursor : type_nets.cursor; -- to be returned

		procedure query_nets (module : in type_module) is
			use type_nets;

			ports : type_port_count;
			net_cursor : type_nets.cursor := module.nets.first;

			use et_schematic.type_ports_netchanger;
			netchanger_cursor : et_schematic.type_ports_netchanger.cursor;
			
			procedure query_netchangers (
			-- Search the net for a netchanger with given index and port
			-- opposide the given port. If netchanger found then netchanger_cursor
			-- points to an element (means it points no longer to no_element).
				net_name	: in type_net_name;
				net			: in type_net) is
				use submodules;
			begin
				netchanger_cursor := find 
					(
					container	=> net.netchangers,
					item		=> 
							(
							index	=> port.index, 
							port	=> opposide_port (port.port)
							)
					);
					-- the given port is a composite of index and port name (master/slave)
			end query_netchangers;

			use submodules;
			
		begin -- query_nets
			while net_cursor /= type_nets.no_element loop
				ports := port_count (net_cursor);

				-- search in the net if it contains netchangers:
				if ports.netchangers.total > 0 then
					
					type_nets.query_element (
						position	=> net_cursor,
						process		=> query_netchangers'access);

				end if;

				-- The search ends once a net containing the opposide port
				-- has been found:
				if netchanger_cursor /= et_schematic.type_ports_netchanger.no_element then
					exit;
				end if;
				
				next (net_cursor);
			end loop;
		end query_nets;
		
	begin -- net_on_netchanger
		type_modules.query_element (module_cursor, query_nets'access);
		return net_cursor;
	end net_on_netchanger;

	function net_in_submodule (
	-- Returns a cursor to the submodule net connected with the given
	-- submodule port.
	-- If the port is not connected inside the submodule then the return is no_element.
		module_cursor	: in type_modules.cursor;
		port			: in et_schematic.type_port_submodule)
		return type_nets.cursor is

		use type_modules;
		net_cursor : type_nets.cursor; -- to be returned

		procedure query_submodules (submodule_cursor : in type_modules.cursor) is
			use et_general.type_module_instance_name;

			procedure query_nets (module : in type_module) is
			-- Search for the net specified by port.port_name. The search ends
			-- once the net has been found. The search is conducted by comparing
			-- with the base names of the nets in the module. The prefix does not matter.
			-- See specs of type_net_name. The base name is something like "output".
			-- The prefix is something like "CLK_GENERATOR/FLT1/". 
			-- But as said above the prefix does not matter here.
				use type_nets;
				use et_general.type_net_name;
			begin
				-- iterate the nets of the module
				net_cursor := module.nets.first;

				while net_cursor /= type_nets.no_element loop

					-- test against the base name:
					if key (net_cursor).base_name = port.port_name then
						exit; -- cancel search with the current net_cursor
					end if;

					next (net_cursor);
				end loop;

				-- If no net found, net_cursor points to no_element.
			end query_nets;
			
		begin -- query_submodules
			if element (submodule_cursor).name = port.module_name then -- submodule found

				-- search in submodule for the net specified by port.port_name:
				query_element (submodule_cursor, query_nets'access);
			end if;
		end query_submodules;
		
	begin -- net_in_submodule
		-- Search among the submodules (one level deeper) for the submodule specified
		-- by port.module_name:
		iterate_children (parent => module_cursor, process => query_submodules'access);
		-- CS: the iteration does not stop after finding the submodule. It is slightly 
		-- a waste of time to query remaining submodules.
		
		return net_cursor;
	end net_in_submodule;
								  
	procedure write_netlist (
	-- Creates the netlist file (which inevitably and intentionally overwrites the previous file).
	-- - modules contains the modules and their nets ordered in a tree structure.
	-- - module_name is the name of the top module. to be written in the header of the netlist file.
	-- - file_name is the name of the actual netlist file.
		modules		: in type_modules.tree;
		module_name		: in type_module_name.bounded_string; -- motor_driver 
		file_name		: in type_file_name.bounded_string; -- netlist.net
		log_threshold	: in type_log_level) is		

		use type_modules;
		netlist_handle : ada.text_io.file_type;

		netlist_cursor : netlists.type_modules.cursor := netlists.type_modules.root (modules);

		package stack is new et_general.stack_lifo (
			item	=> netlists.type_modules.cursor,
			max 	=> submodules.nesting_depth_max);

		procedure write_header is begin
		-- writes a nice header in the netlist file
			put_line (netlist_handle, comment_mark & " " & et_general.system_name & " " & et_general.version & " netlist");
			put_line (netlist_handle, comment_mark & " " & date);
			put_line (netlist_handle, comment_mark & " module " & enclose_in_quotes (to_string (module_name)));
			put_line (netlist_handle, comment_mark & " " & row_separator_double);
-- 			put_line (netlist_handle, comment_mark & " net count total" & count_type'image (length (netlist)));
			-- CS: statistics about pin count ?
			
			put_line (netlist_handle, comment_mark);
			put_line (netlist_handle, comment_mark & " legend:");
			put_line (netlist_handle, comment_mark & "  net_name");
			put_line (netlist_handle, comment_mark & "  device port direction terminal/pin/pad");
			put_line (netlist_handle, comment_mark & " " & row_separator_single);
		end write_header;

		procedure find_dependencies is
		begin
			null;
		end;
			
		procedure write_nets is -- CS for testing only. writes all nets
			
			procedure query_nets (module_cursor : in type_modules.cursor) is 

				procedure query_ports (net_cursor : in type_nets.cursor) is
					use type_nets;

					procedure query_ports (port_cursor : in type_ports.cursor) is
						use type_ports;
					begin
						-- write the device ports
						put_line (netlist_handle, -- IC1 CE input H5
							et_libraries.to_string (element (port_cursor).device) & latin_1.space &
							et_libraries.to_string (element (port_cursor).port) & latin_1.space &
							et_libraries.to_string (element (port_cursor).direction) & latin_1.space &
							et_libraries.to_string (element (port_cursor).terminal) & latin_1.space);
							-- CS .characteristics
					end query_ports;
					
				begin -- query_ports
					-- write the net name
					new_line (netlist_handle);
					
					put_line (netlist_handle, to_string (key (net_cursor).prefix) & 
						to_string (key (net_cursor).base_name)); -- CLK_GENERATOR/FLT1/ & clock_out

					type_ports.iterate (element (net_cursor).devices, query_ports'access);
				end query_ports;
				
			begin -- query_nets
				type_nets.iterate (element (module_cursor).nets, query_ports'access);
			end query_nets;
			 
		begin -- write_nets
			stack.init;
			iterate (modules, query_nets'access);
		end write_nets;
		
-- 		procedure write_nets is
-- 		-- writes the actual nets in the netlist file
-- 
-- 			procedure query_ports (port : in type_ports.cursor) is
-- 				use type_ports;
-- 			begin
-- 				put_line (netlist_handle, -- IC1 CE input H5
-- 					et_libraries.to_string (element (port).device) & latin_1.space &
-- 					et_libraries.to_string (element (port).port) & latin_1.space &
-- 					et_libraries.to_string (element (port).direction) & latin_1.space &
-- 					et_libraries.to_string (element (port).terminal) & latin_1.space);
-- 
-- 					-- CS .characteristics
-- 			end query_ports;
-- 			
-- 			procedure query_nets (net : in type_netlist.cursor) is begin
-- 				new_line (netlist_handle);
-- 
-- 				-- write the net name
-- 				put_line (netlist_handle, to_string (key (net))); -- clock_out
-- 
-- 				-- write the device ports
-- 				iterate (element (net), query_ports'access);
-- 
-- 			end query_nets;
-- 
-- 			
-- 		begin -- write_nets
-- 			iterate (netlist, query_nets'access);
-- 		end write_nets;

		procedure write_footer is begin
		-- writes a nice footer in the netlist file
			new_line (netlist_handle);
			put_line (netlist_handle, comment_mark & " " & row_separator_double);
			put_line (netlist_handle, comment_mark & " end of list");
		end write_footer;
		
	begin -- write_netlist
		if ada.directories.exists (to_string (file_name)) then
			log (importance => NOTE, text => "overwriting " & to_string (file_name) & " ...", level => log_threshold);
		end if;

		if ada.directories.extension (to_string (file_name)) /= extension_netlist then
			log (importance => WARNING, text => "targeted netlist file has no extension " &
				 enclose_in_quotes (extension_netlist) & " !");
		end if;

		log (text => "writing netlist file " & enclose_in_quotes (to_string (file_name)) & " ...", level => log_threshold);
		
		create (
			file => netlist_handle,
			mode => out_file, 
			name => to_string (file_name));

		write_header;
		write_nets;
		write_footer;
		
		close (netlist_handle);

		exception
			when event: others =>
				if is_open (netlist_handle) then
					close (netlist_handle);
				end if;
				
				log_indentation_reset;
				log (text => ada.exceptions.exception_information (event), console => true);
				raise;
				
	end write_netlist;

	
end netlists;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
