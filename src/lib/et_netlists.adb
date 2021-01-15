------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              NETLISTS                                    --
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
with gnat.directory_operations;
with ada.exceptions;

with et_export;

with et_csv;					use et_csv;

package body et_netlists is
	
	function to_string (name : in pac_netlist_file_name.bounded_string) return string is begin
		return pac_netlist_file_name.to_string (name);
	end;
	
	function to_file_name (name : in string) return pac_netlist_file_name.bounded_string is begin
		return pac_netlist_file_name.to_bounded_string (name);
	end;

	function "<" (left, right : in type_device_port_extended) return boolean is
		use et_symbols.pac_port_name;
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

	function "<" (left, right : in type_submodule_port_extended) return boolean is
		use pac_module_instance_name;
		use pac_net_name;
		result : boolean := false;
	begin
		if left.module < right.module then
			result := true;
			
		elsif left.module = right.module then

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
	
	function to_prefix (instance : in pac_module_instance_name.bounded_string) -- OSC1
		return pac_net_name.bounded_string is
	begin
		return to_net_name (to_string (instance) & level_separator);
	end;

	function "<" (left, right : in type_port_netchanger) return boolean is begin
		if left.index < right.index then
			return true;
		elsif left.index > right.index then
			return false;
		elsif left.port < right.port then
			return true;
		else
			return false;
		end if;
	end;

	
	function to_string (net_scope : in type_net_scope) return string is begin
		return " " & to_lower (type_net_scope'image (net_scope));
	end to_string;

	function to_net_scope (scope : in string) return type_net_scope is begin
		return type_net_scope'value (scope);
	end to_net_scope;

	
	procedure log_net_name (
		name			: in type_net_name;
		primary			: in boolean;
		log_threshold	: in type_log_level) is
	begin
		if primary then
			log (
				text => "primary net " & enclose_in_quotes (
					to_string (name.prefix) & 
					to_string (name.base_name)), -- CLK_GENERATOR/FLT1/ & clock_out
				level => log_threshold);

		else
			log (
				text => "secondary net " & enclose_in_quotes (
					to_string (name.prefix) & 
					to_string (name.base_name)), -- CLK_GENERATOR/FLT1/ & clock_out
				level => log_threshold);

		end if;
	end log_net_name;

	
	function "<" (left, right : in type_net_name) return boolean is
		result : boolean := false;
		use pac_net_name;
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

	function port_count (net_cursor : in pac_nets.cursor)
		return type_port_count is
	-- Returns the number of netchanger and submodule ports in the given net.

		port_count : type_port_count; -- to be returned

		procedure query_ports (
			net_name	: in type_net_name;
			net			: in type_net) is
			use pac_submodule_ports_extended;
			use pac_netchanger_ports;

			procedure count_netchanger_ports (cursor : in pac_netchanger_ports.cursor) is begin
				case element (cursor).port is
					when MASTER => port_count.netchangers.masters := port_count.netchangers.masters + 1;
					when SLAVE => port_count.netchangers.slaves := port_count.netchangers.slaves + 1;
				end case;
			end;

			procedure count_submodule_ports (cursor : in pac_submodule_ports_extended.cursor) is begin
				case element (cursor).direction is
					when MASTER => port_count.submodules.masters := port_count.submodules.masters + 1;
					when SLAVE => port_count.submodules.slaves := port_count.submodules.slaves + 1;
				end case;
			end;
			
		begin -- query_ports
			-- The number of master or slave ports
			-- requires iterating and dividing masters from slaves:

			-- submodules
			iterate (net.submodules, count_submodule_ports'access);

			port_count.submodules.total := port_count.submodules.masters +
											port_count.submodules.slaves;

			
			-- netchangers
			iterate (net.netchangers, count_netchanger_ports'access);

			port_count.netchangers.total := port_count.netchangers.masters +
											port_count.netchangers.slaves;
			
		end query_ports;
			
	begin -- port_count
		pac_nets.query_element (
			position	=> net_cursor,
			process		=> query_ports'access);
		
		return port_count;
	end port_count;

	function is_primary (net_cursor : in pac_nets.cursor) return boolean is
	-- Returns true if given net is a primary net.
	-- Performs some other important checks on slave ports of netchangers and submodules.
	-- CS Currently these test are very simple and should be refined.
		ports : type_port_count;
		
		result : boolean := false; -- CS is this a good safety measure ?

		use pac_nets;
		
-- 		result_on_netchangers : boolean;
-- 		result_on_submodules : boolean;

-- 		procedure contention_by_netchangers is begin
-- 			log (importance => ERROR, text => "net name contention caused by multiple netchanger slave ports !");
-- 			raise constraint_error;
-- 		end;
-- 
-- 		procedure contention_by_submodules is begin
-- 			log (importance => ERROR, text => "net name contention caused by multiple submodule slave ports !");
-- 			raise constraint_error;
-- 		end;

		procedure contention_by_both is begin
			log (importance => ERROR, text => "net name contention caused by multiple netchanger or submodule slave ports !");

			-- CS: list affected ports
			raise constraint_error;
		end;
		
	begin
		ports := port_count (net_cursor);

-- 		-- Test the number of netchanger slave ports:
-- 		
-- 		-- Zero slave ports means: it is a primary net.
-- 		-- One slave port means: it is not a primary net.
-- 		-- More slave ports means: contention -> error.
-- 		case ports.netchangers.slaves is
-- 			when 0 => result_on_netchangers := true;
-- 			when 1 => result_on_netchangers := false;
-- 			when others => contention_by_netchangers;
-- 		end case;
-- 
-- 		-- Test the number of submodule slave ports:
-- 		
-- 		-- Zero slave ports means: it is a primary net.
-- 		-- One slave port means: it is not a primary net.
-- 		-- More slave ports means: contention -> error.
-- 		case ports.submodules.slaves is
-- 			when 0 => result_on_submodules := true;
-- 			when 1 => result_on_submodules := false;
-- 			when others => contention_by_submodules;
		-- 		end case;

		case element (net_cursor).scope is
			when LOCAL =>
		
				-- Test the sum of netchanger and submodule slave ports:
				case natural (ports.netchangers.slaves) + natural (ports.submodules.slaves) is
					when 0 => result := true;
					when 1 => result := false;
					when others => contention_by_both;
				end case;

			when GLOBAL =>

				-- If the net is global, means it can be connected with a same named net
				-- in the parent module without any netchangers, it is secondary:
				result := false;

		end case;
		
		return result;
	end is_primary;

	-- Returns true if net (indicated by net_cursor) is connected with the
	-- given port of a submodule instance.
	function contains (
		net_cursor		: in pac_nets.cursor;
		submodule		: in pac_module_instance_name.bounded_string; -- OSC1
		port			: in pac_net_name.bounded_string) -- clock_out
		return boolean 
	is
		result : boolean := false;
		
		use pac_nets;

		procedure query_submod_ports (
			net_name	: in type_net_name;
			net			: in type_net) 
		is
			use pac_submodule_ports_extended;
			port_cursor : pac_submodule_ports_extended.cursor := net.submodules.first;
			use pac_net_name;
			use et_general.pac_module_instance_name;
		begin
			while port_cursor /= pac_submodule_ports_extended.no_element loop
				
				if element (port_cursor).module = submodule -- OSC1
				and element (port_cursor).port = port then -- clock_out
					result := true;
					exit;
				end if;
				
				next (port_cursor);
			end loop;
		end query_submod_ports;
										 
	begin -- contains
		query_element (
			position	=> net_cursor,
			process		=> query_submod_ports'access);

		return result;
	end contains;
	
	function global_nets_in_submodules (
	-- Returns a list of cursors to same named nets in submodules.
		module_cursor	: in pac_modules.cursor; -- the module that contains the port
		net_cursor		: in pac_nets.cursor;
		log_threshold	: in type_log_level)
		return pac_global_nets.list
	is
		use pac_modules;
		use pac_nets;
		
		global_nets : pac_global_nets.list; -- to be returned

		procedure query_submodules (submodule_cursor : in pac_modules.cursor) is
			use et_general.pac_module_instance_name;

			procedure query_nets (module : in type_module) is
			-- Search for a global net named after the given net (via net_cursor).
			-- The serach ends once the net has been found. The search is conducted by comparing
			-- with the base names of the nets in the module. The cursor to the net and 
			-- the name of the generic module is then
			-- appended to the list net_cursors (to be returned).
				use pac_net_name;
				cursor : pac_nets.cursor := module.nets.first;
			begin
				-- iterate the nets of the module
				while cursor /= pac_nets.no_element loop

					-- the net must be a global net:
					if element (cursor).scope = GLOBAL then
						
						-- test against the base name:
						if key (cursor).base_name = key (net_cursor).base_name then

							-- Append the global net to the global_nets:
							pac_global_nets.append 
								(
								container	=> global_nets,
								new_item	=> (
									submodule	=> submodule_cursor,
									net			=> cursor)			-- the global net within the submodule
								);
							
							exit; -- no need for more searching (module.nets is a map and the net occurs in it only once)
						end if;
					end if;
					
					next (cursor);
				end loop;

				-- If no net found, net_cursor points to no_element.
			end query_nets;
			
		begin -- query_submodules
-- 			log (text => "submodule " &
-- 					enclose_in_quotes (to_string (pac_modules.element (submodule_cursor).generic_name)),
-- 				level => log_threshold);
				
			-- search in submodule for a net named after the given net (via net_cursor):
			query_element (submodule_cursor, query_nets'access);

		end query_submodules;
		
	begin -- global_nets_in_submodules
		log_indentation_up;
		
		log (text => "searching global secondary nets of net " &
			  enclose_in_quotes (to_string (key (net_cursor).base_name)) & " in submodules ...",
			level => log_threshold);
		
		log_indentation_up;
		
		-- Iterate the submodules (one level deeper):
		iterate_children (parent => module_cursor, process => query_submodules'access);

		log_indentation_down;
		log_indentation_down;		
		return global_nets;
	end global_nets_in_submodules;
	
	function net_on_netchanger (
	-- Returns a cursor to the net connected with the given netchanger
	-- port opposide to the given port.
	-- If the given port is a master, then the net connected with the
	-- slave is returned (and vice versa).
	-- If the netchanger is not connected then the return is no_element.
		module_cursor	: in pac_modules.cursor; -- the module that contains the port
		port			: in type_port_netchanger;
		log_threshold	: in type_log_level)
		return pac_nets.cursor is

		use pac_nets;
		net_cursor : pac_nets.cursor; -- to be returned

		procedure query_nets (module : in type_module) is

			ports : type_port_count;

			use pac_netchanger_ports;
			netchanger_cursor : pac_netchanger_ports.cursor;
			
			procedure query_netchangers (
			-- Search the net for a netchanger with given index and port
			-- opposide the given port. If netchanger found then netchanger_cursor
			-- points to an element (means it points no longer to no_element).
				net_name	: in type_net_name;
				net			: in type_net) is
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
			
		begin -- query_nets
			log_indentation_up;

			net_cursor := module.nets.first;
			while net_cursor /= pac_nets.no_element loop
				ports := port_count (net_cursor);

				-- search in the net if it contains netchangers:
				if ports.netchangers.total > 0 then

					--log (text => to_string (key (net_cursor).base_name), level => log_threshold + 1);
					
					pac_nets.query_element (
						position	=> net_cursor,
						process		=> query_netchangers'access);

				end if;

				-- The search ends once a net containing the opposide port
				-- has been found:
				if netchanger_cursor /= pac_netchanger_ports.no_element then
					exit;
				end if;
				
				next (net_cursor);
			end loop;

			log_indentation_down;
		end query_nets;
		
	begin -- net_on_netchanger
		log_indentation_up;
		
		log (text => "searching secondary net connected via netchanger in module " &
			enclose_in_quotes (to_string (pac_modules.element (module_cursor).generic_name)),
				level => log_threshold);

		-- iterate nets inside the given module
		pac_modules.query_element (module_cursor, query_nets'access);

		if net_cursor = pac_nets.no_element then
			log (text => " none found", level => log_threshold);
		else
			log (text => " secondary net " & enclose_in_quotes (to_string (key (net_cursor).base_name)),
				level => log_threshold);
		end if;
		
		log_indentation_down;
		return net_cursor;
	end net_on_netchanger;

	function net_in_submodule (
	-- Returns a cursor to the submodule net connected with the given
	-- submodule port.
	-- If the port is not connected inside the submodule then the return is no_element.
		module_cursor	: in pac_modules.cursor; -- the module that contains the port
		port			: in type_submodule_port_extended;
		log_threshold	: in type_log_level)
		return pac_nets.cursor 
	is
		use pac_modules;
		use pac_nets;
		net_cursor : pac_nets.cursor; -- to be returned

		procedure query_submodules (submodule_cursor : in pac_modules.cursor) is
			use et_general.pac_module_instance_name;

			procedure query_nets (module : in type_module) is
			-- Search for the net specified by "port.port". The search ends
			-- once the net has been found. The search is conducted by comparing
			-- with the base names of the nets in the module. The prefix does not matter.
			-- See specs of type_net_name. The base name is something like "output".
			-- The prefix is something like "CLK_GENERATOR/FLT1/". 
			-- But as said above the prefix does not matter here.
				use pac_net_name;
			begin
				-- iterate the nets of the module
				net_cursor := module.nets.first;

				while net_cursor /= pac_nets.no_element loop

					-- test against the base name:
					if key (net_cursor).base_name = port.port then
						exit; -- cancel search with the current net_cursor
					end if;

					next (net_cursor);
				end loop;

				-- If no net found, net_cursor points to no_element.
			end query_nets;
			
		begin -- query_submodules
			if element (submodule_cursor).instance_name = port.module then -- submodule found
				
				log (text => "searching local secondary net in submodule " &
					enclose_in_quotes (to_string (pac_modules.element (submodule_cursor).generic_name)),
						level => log_threshold);
				
				-- search in submodule for the net specified by port.port_name:
				query_element (submodule_cursor, query_nets'access);

			end if;
		end query_submodules;
		
	begin -- net_in_submodule
		log_indentation_up;
		
		-- Search among the submodules (one level deeper) for the submodule specified
		-- by port.module_name:
		iterate_children (parent => module_cursor, process => query_submodules'access);
		-- CS: the iteration does not stop after finding the submodule. It is slightly 
		-- a waste of time to query remaining submodules.

		if net_cursor = pac_nets.no_element then
			log (text => " none found -> not connected", level => log_threshold);
		else
			log (text => " secondary net " & enclose_in_quotes (to_string (key (net_cursor).base_name)),
				level => log_threshold);
		end if;

		log_indentation_down;
		return net_cursor;
	end net_in_submodule;

	function net_in_parent_module (
	-- Returns a cursor to the net in the parent module connected with the given net.
	-- Searches for a net in the parent module that is connected to the submodule instance
	-- given by element (module_cursor).instance_name, a port named after key (net_cursor).base_name
	-- and port direction "slave".
	-- If the net is in the top module, then the return is no_element.
	-- If the net is not connected in the parent module (via the port in the box representing
	-- the submodule instance) then the return is no_element.
		module_cursor	: in pac_modules.cursor; -- the module that contains the net
		net_cursor		: in pac_nets.cursor;
		log_threshold	: in type_log_level)
		return pac_nets.cursor is

		use pac_modules;
		use pac_nets;
		net_cursor_parent : pac_nets.cursor; -- to be returned

		-- Get the cursor to the parent module. If module_cursor points to the top
		-- module, then parent_module_cursor will point to root.
		parent_module_cursor : pac_modules.cursor := parent (module_cursor);

		port_to_search_for : type_submodule_port_extended; -- the submodule port we are looking for
		port_found : boolean := false; -- signals loop in procedure query_nets to cancel the search
		
		procedure query_submodules (
			net_name	: in type_net_name;
			net			: in type_net) is
			use pac_submodule_ports_extended;
			port_cursor : pac_submodule_ports_extended.cursor;
		begin
			-- Locate the submodule port in the current net.
			-- If found, the search ends prematurely.
			port_cursor := find (net.submodules, port_to_search_for);
			if port_cursor /= pac_submodule_ports_extended.no_element then
				port_found := true;
			end if;
		end query_submodules;
		
		procedure query_nets (module : in type_module) is
			net_cursor : pac_nets.cursor := module.nets.first;
		begin
			-- iterate nets of module. cancel search once the desired port has been found
			while net_cursor /= pac_nets.no_element loop

				pac_nets.query_element (
					position	=> net_cursor,
					process		=> query_submodules'access);

				if port_found then
					-- set the cursor to be returned
					net_cursor_parent := net_cursor;
					exit;
				end if;
				
				next (net_cursor);
			end loop;
		end query_nets;
		
	begin -- net_in_parent_module
		if is_root (parent_module_cursor) then -- this is the top module
			-- there is no net
			net_cursor_parent := pac_nets.no_element;
		else
			log_indentation_up;
			
			log (text => "searching secondary net in parent module " &
					enclose_in_quotes (to_string (element (parent_module_cursor).generic_name)),
				level => log_threshold);

			-- Build the submodule port to be searched for.
			port_to_search_for := (
				module		=> element (module_cursor).instance_name, -- MOT_DRV_2
				port		=> pac_nets.key (net_cursor).base_name,  -- clock_out
				direction	=> SLAVE);

			-- iterate in nets of parent module
			query_element (parent_module_cursor, query_nets'access);

			if net_cursor_parent = pac_nets.no_element then
				log (text => " none found -> not connected", level => log_threshold);
			else
				log (text => " secondary net " & enclose_in_quotes (to_string (key (net_cursor_parent).base_name)),
					 level => log_threshold);
			end if;
			
			log_indentation_down;
		end if;
		
		return net_cursor_parent;
	end net_in_parent_module;

	procedure write_netlist (
	-- Exports the netlist of the given module to the export/CAM directory.
		netlist			: in pac_netlist.tree;
		module_name		: in pac_module_name.bounded_string; -- motor_driver 
		variant_name	: in et_general.pac_assembly_variant_name.bounded_string; -- low_cost
		log_threshold	: in type_log_level) is

		file_name : pac_netlist_file_name.bounded_string;
		
		procedure set_file_name is 
			use ada.directories;
			use gnat.directory_operations;
			use pac_module_name;
			use et_general.pac_assembly_variant_name;
			use et_export;
		begin
			if is_default (variant_name) then
				file_name := to_file_name (
							compose 
							(
								containing_directory	=> directory_export & dir_separator & directory_cam &
															dir_separator & directory_netlists,

								name					=> et_general.to_string (module_name),
								extension				=> extension_netlist
							));

			else
				file_name := to_file_name (
							compose 
							(
								containing_directory	=> directory_export & dir_separator & directory_cam &
															dir_separator & directory_netlists,

								name					=> et_general.to_string (module_name) & "_" & 
															to_variant (variant_name),
								extension				=> extension_netlist
							));
			end if;
		end;	
		
		netlist_handle : ada.text_io.file_type;

		procedure write_header is 
			use pac_netlist;
			netlist_cursor : pac_netlist.cursor := root (netlist);
		begin
		-- writes a nice header in the netlist file
			put_line (netlist_handle, comment_mark & " " & et_general.system_name & " " & et_general.version & " netlist");
			put_line (netlist_handle, comment_mark & " " & date);
			put_line (netlist_handle, comment_mark & " module " & enclose_in_quotes (to_string (module_name)));
			put_line (netlist_handle, comment_mark & " " & row_separator_double);
			put_line (netlist_handle, comment_mark & " net count total" & count_type'image (child_count (netlist_cursor)));
			-- CS: statistics about pin count ?
			
			put_line (netlist_handle, comment_mark);
			put_line (netlist_handle, comment_mark & " legend:");
			put_line (netlist_handle, comment_mark & "  net_name");
			put_line (netlist_handle, comment_mark & "  device port direction terminal/pin/pad");
			put_line (netlist_handle, comment_mark);
			put_line (netlist_handle, comment_mark & "  Names of secondary nets are comments.");
			put_line (netlist_handle, comment_mark & " " & row_separator_single);
		end write_header;
		
		procedure write_footer is begin
		-- writes a nice footer in the netlist file
			new_line (netlist_handle);
			put_line (netlist_handle, comment_mark & " " & row_separator_double);
			put_line (netlist_handle, comment_mark & " end of list");
		end write_footer;

		procedure write_nets is 
			use pac_netlist;
			use et_terminals;

			procedure query_device (port_cursor : in pac_device_ports_extended.cursor) is
			-- Writes the device port in the netlist file.
				use pac_device_ports_extended;
				use et_symbols;
			begin
				put_line (netlist_handle, -- IC1 CE input H5
					to_string (element (port_cursor).device) & latin_1.space &
					to_string (element (port_cursor).port) & latin_1.space &
					to_string (element (port_cursor).direction) & latin_1.space &
					to_string (element (port_cursor).terminal) & latin_1.space);
					-- CS .characteristics
			end;

			procedure query_secondary_net (net_cursor : in pac_netlist.cursor) is begin
				-- Extract the ports of devices of the secondary net:

				log_indentation_up;
				log_net_name (element (net_cursor).name, false, log_threshold + 1);

				-- write the secondary net name as comment:
				new_line (netlist_handle);
				put_line (netlist_handle, comment_mark & latin_1.space &
					to_string (element (net_cursor).name.prefix) & 
					to_string (element (net_cursor).name.base_name)); -- CLK_GENERATOR/FLT1/ & clock_out
				
				pac_device_ports_extended.iterate (element (net_cursor).devices, query_device'access);

				-- Iterate secondary nets:
				iterate_children (net_cursor, query_secondary_net'access);

				log_indentation_down;
			end;
			
			procedure write_primary_net (net_cursor : in pac_netlist.cursor) is begin
				log_indentation_up;
				log_net_name (element (net_cursor).name, true, log_threshold + 1);
				
				-- write the primary net name:
				new_line (netlist_handle);
				put_line (netlist_handle, comment_mark & " -------");
				put_line (netlist_handle, to_string (element (net_cursor).name.prefix) & 
					to_string (element (net_cursor).name.base_name)); -- CLK_GENERATOR/FLT1/ & clock_out

				-- Extract the ports of devices of the primary net:
				pac_device_ports_extended.iterate (element (net_cursor).devices, query_device'access);

				-- Iterate secondary nets:
				iterate_children (net_cursor, query_secondary_net'access);

				log_indentation_down;
			end write_primary_net;
			
		begin -- write_nets
			iterate_children (root (netlist), write_primary_net'access);
		end write_nets;
		
	begin -- write_netlist
		set_file_name;
	
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
		
	function make_netlist (
	-- If write_file ist true, creates the netlist file (which inevitably and intentionally 
	-- overwrites the previous file).
	-- - modules contains the modules and their nets ordered in a tree structure.
	-- - module_name is the name of the top module. to be written in the header of the netlist file.
	-- - The netlist file will be named after the module name and the assembly variant.
	-- - Exports the netlist of the given module to the export/CAM directory.							  
		modules			: in pac_modules.tree;
		module_name		: in pac_module_name.bounded_string; -- motor_driver 
		variant_name	: in et_general.pac_assembly_variant_name.bounded_string; -- low_cost
		write_file		: in boolean;
		log_threshold	: in type_log_level)
		return pac_netlist.tree
	is
		use pac_modules;
		
		use pac_netlist;
		netlist : pac_netlist.tree; -- to be returned

		-- While exploring the tree of modules (with their individual netlists) the
		-- cursor in container "netlist" points to a certain net. This cursor must be 
		-- backup on a stack (see below) each time we dive into secondary nets.
		netlist_cursor : pac_netlist.cursor := pac_netlist.root (netlist);
			
		-- When exploring secondary nets, the cursor to a net must be backup.
		-- This must be done each time a secondary net is discovered.
		package stack is new et_general.stack_lifo (
			item	=> pac_netlist.cursor,
			max 	=> nesting_depth_max);

		procedure find_dependencies ( -- prespecification only. see body below.
			module_cursor	: in pac_modules.cursor;
			net_cursor		: in pac_nets.cursor;
			log_threshold	: in type_log_level);
		
		procedure query_global_nets_in_submodules (
		-- Explores global secondary nets of given net in submodules of given module.
		-- If the given module does not have submodules, nothing happens.
		-- Calls find_dependencies.
			module_cursor	: in pac_modules.cursor;
			net_cursor		: in pac_nets.cursor;
			log_threshold	: in type_log_level) is

			procedure query_submodules is 
				use pac_global_nets;
				global_nets : pac_global_nets.list;
				
				procedure query_nets (cursor : in pac_global_nets.cursor) is
					use pac_nets;
					glob_net : type_global_net := element (cursor);
				begin
					-- glob_net is a record providing a cursor to a submodule and
					-- a cursor to a net therein.

					-- backup netlist cursor before diving into secondary nets
					stack.push (netlist_cursor);

					pac_netlist.insert_child (
						container	=> netlist,
						parent		=> netlist_cursor,
						before		=> pac_netlist.no_element,
						position	=> netlist_cursor,
						new_item	=> (element (glob_net.net) with key (glob_net.net)));
					
					-- CS: Make sure the net is not connected via a port with the parent module.
-- 					if not contains (
-- 						net_cursor		=> net_cursor,
-- 						submodule		=> element (glob_net.submodule).instance_name, -- OSC1
-- 						port			=> key (glob_net.net).base_name) then -- clock_out

					log (text => "submodule " &
						enclose_in_quotes (to_string (pac_modules.element (glob_net.submodule).generic_name)) &
						" net " & enclose_in_quotes (to_string (key (glob_net.net).base_name)),
						level => log_threshold);

					-- Start exploring the net indicated by glob_net:
					find_dependencies (glob_net.submodule, glob_net.net, log_threshold);

					-- restore netlist cursor as it was before diving into the secondary net
					netlist_cursor := stack.pop;
						
-- 					else
-- 						log (ERROR, "net " & enclose_in_quotes (to_string (key (glob_net.net).base_name)) &
-- 							 " is already connected with its parent module via a port. Remove port in " &
-- 							 "parent module or set scope of this net to local !");
-- 						raise constraint_error;
-- 					end if;
				end query_nets;
				
			begin -- query_submodules
				-- Get all global secondary nets of submodules:
				global_nets := global_nets_in_submodules (module_cursor, net_cursor, log_threshold);

				log_indentation_up;
				log_indentation_up;
				
				if is_empty (global_nets) then
					log (text => "no global secondary nets found", level => log_threshold);
				else
					iterate (global_nets, query_nets'access);
				end if;

				log_indentation_down;
				log_indentation_down;
			end query_submodules;

		begin -- query_global_nets_in_submodules
			if child_count (module_cursor) > 0 then
				query_submodules;
			end if;
		end query_global_nets_in_submodules;
		
		procedure find_dependencies (
		-- Explores secondary nets starting with the given net in the given module.
		-- NOTE: This procedure is recursive, means it calls itself until all
		--       secondary nets have been found.
			module_cursor	: in pac_modules.cursor; -- the module we are in
			net_cursor		: in pac_nets.cursor; -- the net we are in
			log_threshold	: in type_log_level) is
			
			use pac_nets;
			
			-- In order to save computing time, get number of netchanger and submodule 
			-- ports of this net before exploring the net.
			ports : type_port_count := port_count (net_cursor);
			-- ports now provides the number of submodule and netchanger ports
				
			procedure query_netchanger (port_cursor : in pac_netchanger_ports.cursor) is
				use pac_netchanger_ports;
				net_cursor : pac_nets.cursor;
			begin
				if element (port_cursor).port = MASTER then

					-- Find the secondary net connected with the slave port:
					net_cursor := net_on_netchanger (module_cursor, element (port_cursor), log_threshold + 1);

					-- If there is a secondary net, dive into it and find further 
					-- secondary nets in this module. Otherwise the netchanger is not connected
					-- on its slave end:
					if net_cursor /= pac_nets.no_element then
						
						-- net_cursor now points to the secondary net in the same module
						log_indentation_up;
						log_net_name (key (net_cursor), false, log_threshold + 1);

						-- backup netlist cursor before diving into secondary nets
						stack.push (netlist_cursor);
						
						pac_netlist.insert_child (
							container	=> netlist,
							parent		=> netlist_cursor,
							before		=> pac_netlist.no_element,
							position	=> netlist_cursor,
							new_item	=> (element (net_cursor) with key (net_cursor)));

						-- dive into secondary nets
						find_dependencies (module_cursor, net_cursor, log_threshold + 1);

						-- restore netlist cursor as it was before diving into the secondary net
						netlist_cursor := stack.pop;

						log_indentation_down;
					end if;
				end if;
			end query_netchanger;

			procedure query_submodule (port_cursor : in pac_submodule_ports_extended.cursor) is
				use pac_submodule_ports_extended;
				net_cursor : pac_nets.cursor;
			begin
				if element (port_cursor).direction = MASTER then

					-- Find the secondary net in the submodule:
					net_cursor := net_in_submodule (module_cursor, element (port_cursor), log_threshold + 1);

					-- If there is a secondary net, dive into it and find further 
					-- secondary nets in the submodule. Otherwise the port is not connected
					-- inside the submodule:
					if net_cursor /= pac_nets.no_element then
						
						-- net_cursor now points to the secondary net in the submodule
						log_indentation_up;
						log_net_name (key (net_cursor), false, log_threshold + 1);

						-- backup netlist cursor before diving into secondary nets
						stack.push (netlist_cursor);
						
						pac_netlist.insert_child (
							container	=> netlist,
							parent		=> netlist_cursor,
							before		=> pac_netlist.no_element,
							position	=> netlist_cursor,
							new_item	=> (element (net_cursor) with key (net_cursor)));

						-- dive into secondary nets
						find_dependencies (module_cursor, net_cursor, log_threshold + 1);

						-- restore netlist cursor as it was before diving into the secondary net
						netlist_cursor := stack.pop;

						log_indentation_down;
					end if;
				end if;
			end query_submodule;

			procedure query_parent is
				use pac_submodule_ports_extended;
				cursor : pac_nets.cursor;
			begin
				-- Find the secondary net in the parent module.
				cursor := net_in_parent_module (module_cursor, net_cursor, log_threshold + 1);

				-- If there is a secondary net, dive into it and find further secondary nets
				-- in the parent module. Otherwise the port is not connected in the parent module
				-- or there is no parent module at all (because module_cursor is pointing at the top module):
				if cursor /= pac_nets.no_element then
					
					-- cursor now points to the secondary net in the parent module

					-- backup netlist cursor before diving into secondary nets
					stack.push (netlist_cursor);

					pac_netlist.insert_child (
						container	=> netlist,
						parent		=> netlist_cursor,
						before		=> pac_netlist.no_element,
						position	=> netlist_cursor,
						new_item	=> (element (cursor) with key (cursor)));
					
					-- dive into secondary nets
					find_dependencies (module_cursor, cursor, log_threshold + 1);

					-- restore netlist cursor as it was before diving into the secondary net
					netlist_cursor := stack.pop;
				end if;
			end query_parent;

		begin -- find_dependencies
			-- Now we must extract the ports of netchangers and submodules in the given parent net. Since we
			-- want to explore secondary nets, the next steps are required if there are netchangers
			-- with master ports. A netchanger master port is THE bridge to a secondary net. The secondary
			-- net is connected with the slave port, regardless whether the secondary net is in the
			-- same module, in the parent module or in a submodule.
			
			if ports.netchangers.masters > 0 then
				-- If there are netchangers connected with the net, look at their ports and the nets connected.
				-- Since we want to explore secondary nets, this step addresses netchanger ports of direction "master".
				-- This search REMAINS on the CURRENT level of design hierarchy:
				pac_netchanger_ports.iterate (element (net_cursor).netchangers, query_netchanger'access);

				-- The parent module must be searched for submodule instances (the boxes) where
				-- the net surfaces as a port. Even in a parent module the name of a secondary net may be
				-- dictated by the primary net in a submodule.
				-- Probe the net whether it is connected with a secondary net in the parent module.
				-- This search goes UP in the design hierarchy towards the parent module:
				query_parent;
			end if;
			
			if ports.submodules.masters > 0 then
				-- If there are submodules connected with the net, look at their ports and the nets connected.
				-- Since we want to explore secondary nets, this step adresses submodue ports of direction "master".
				-- This search goes DOWN in the design hierarchy towards the submodule:
				pac_submodule_ports_extended.iterate (element (net_cursor).submodules, query_submodule'access);
			end if;

			-- There may be submodules containing global nets. These nets are secondary nets of the current net.
			-- Locate those nets if the current module has submodules (children):
			query_global_nets_in_submodules (module_cursor, net_cursor, log_threshold + 1);
			
		end find_dependencies;
			
		procedure explore_nets is
			
			procedure query_nets (module_cursor : in pac_modules.cursor) is 

				procedure query_ports (net_cursor : in pac_nets.cursor) is
					use pac_nets;
				begin
					-- Extract primary nets only:
					if is_primary (net_cursor) then

						log_indentation_up;

						log_net_name (key (net_cursor), true, log_threshold + 1);

						-- backup netlist cursor before diving into secondary nets
						stack.push (netlist_cursor);
						
						pac_netlist.insert_child (
							container	=> netlist,
							parent		=> root (netlist),
							before		=> pac_netlist.no_element,
							position	=> netlist_cursor,
							new_item	=> (element (net_cursor) with key (net_cursor)));
						
						-- write device ports and dive into secondary nets
						find_dependencies (module_cursor, net_cursor, log_threshold + 2);

						-- restore netlist cursor as it was before diving into the secondary net
						netlist_cursor := stack.pop;
						
						log_indentation_down;
					end if;

				end query_ports;
				
			begin -- query_nets
				pac_nets.iterate (element (module_cursor).nets, query_ports'access);
			end query_nets;
			 
		begin -- explore_nets
			stack.init;
			iterate (modules, query_nets'access);
		end explore_nets;
		
	begin -- make_netlist
		explore_nets;

		if write_file then
			write_netlist (netlist, module_name, variant_name, log_threshold);
		end if;
		
		return netlist;
				
	end make_netlist;

	
end et_netlists;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
