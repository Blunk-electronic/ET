------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              NETLISTS                                    --
--                                                                          --
--                               B o d y                                    --
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
-- To Do: 
--
--
--


with ada.text_io;				use ada.text_io;
with ada.exceptions;




package body et_netlists is
	
	use pac_net_name;
	
	

	function "<" (left, right : in type_device_port_extended) return boolean is
		use pac_port_name;
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



	
	


	
	procedure log_net_name (
		name			: in type_net_name;
		primary			: in boolean;
		log_threshold	: in type_log_level)
	is begin
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





	
	
	
	function get_port_count (
		net_cursor : in pac_nets.cursor)
		return type_port_count 
	is
		port_count : type_port_count; -- to be returned

		
		procedure query_ports (
			net_name	: in type_net_name;
			net			: in type_net) 
		is
			use pac_submodule_ports_extended;
			use pac_netchanger_ports;

			
			procedure count_netchanger_ports (
				cursor : in pac_netchanger_ports.cursor) 
			is begin
				case element (cursor).port is
					when MASTER => port_count.netchangers.masters := port_count.netchangers.masters + 1;
					when SLAVE => port_count.netchangers.slaves := port_count.netchangers.slaves + 1;
				end case;
			end;

			
			procedure count_submodule_ports (
				cursor : in pac_submodule_ports_extended.cursor) 
			is begin
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
			
	begin
		pac_nets.query_element (
			position	=> net_cursor,
			process		=> query_ports'access);
		
		return port_count;
	end get_port_count;



	


	
	
	function is_primary (net_cursor : in pac_nets.cursor) return boolean is
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
		ports := get_port_count (net_cursor);

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
			use pac_module_instance_name;
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
		module_cursor	: in pac_netlist_modules.cursor; -- the module that contains the port
		net_cursor		: in pac_nets.cursor;
		log_threshold	: in type_log_level)
		return pac_global_nets.list
	is

		use pac_nets;
		
		global_nets : pac_global_nets.list; -- to be returned

		
		procedure query_submodules (submodule_cursor : in pac_netlist_modules.cursor) is
			use pac_module_instance_name;

			procedure query_nets (module : in type_netlist_module) is
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
-- 					enclose_in_quotes (to_string (pac_netlist_modules.element (submodule_cursor).generic_name)),
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
		module_cursor	: in pac_netlist_modules.cursor; -- the module that contains the port
		port			: in type_port_netchanger;
		log_threshold	: in type_log_level)
		return pac_nets.cursor 
	is
		use pac_nets;
		net_cursor : pac_nets.cursor; -- to be returned

		
		procedure query_nets (
			module : in type_netlist_module) 
		is

			ports : type_port_count;

			use pac_netchanger_ports;
			netchanger_cursor : pac_netchanger_ports.cursor;
			
			
			-- Search the net for a netchanger with given index and port
			-- opposide the given port. If netchanger found then netchanger_cursor
			-- points to an element (means it points no longer to no_element).
			procedure query_netchangers (
				net_name	: in type_net_name;
				net			: in type_net) 
			is begin
				netchanger_cursor := find 
					(
					container	=> net.netchangers,
					item		=> 
							(
							index	=> port.index, 
							port	=> get_opposide_port (port.port)
							)
					);
					-- the given port is a composite of index and port name (master/slave)
			end query_netchangers;
			
			
		begin -- query_nets
			log_indentation_up;

			net_cursor := module.nets.first;
			while net_cursor /= pac_nets.no_element loop
				ports := get_port_count (net_cursor);

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
		
		
	begin
		log_indentation_up;
		
		log (text => "searching secondary net connected via netchanger in module " &
			enclose_in_quotes (to_string (pac_netlist_modules.element (module_cursor).generic_name)),
				level => log_threshold);

		-- iterate nets inside the given module
		query_element (module_cursor, query_nets'access);

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
		module_cursor	: in pac_netlist_modules.cursor; -- the module that contains the port
		port			: in type_submodule_port_extended;
		log_threshold	: in type_log_level)
		return pac_nets.cursor 
	is
		use pac_nets;
		net_cursor : pac_nets.cursor; -- to be returned

		
		procedure query_submodules (submodule_cursor : in pac_netlist_modules.cursor) is
			use pac_module_instance_name;

			
			procedure query_nets (module : in type_netlist_module) is
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
					enclose_in_quotes (to_string (element (submodule_cursor).generic_name)),
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
		module_cursor	: in pac_netlist_modules.cursor; -- the module that contains the net
		net_cursor		: in pac_nets.cursor;
		log_threshold	: in type_log_level)
		return pac_nets.cursor 
	is
		use pac_netlist_modules;
		use pac_nets;
		net_cursor_parent : pac_nets.cursor; -- to be returned

		-- Get the cursor to the parent module. If module_cursor points to the top
		-- module, then parent_module_cursor will point to root.
		parent_module_cursor : pac_netlist_modules.cursor := parent (module_cursor);

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
		
		
		procedure query_nets (module : in type_netlist_module) is
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



	
end et_netlists;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
