------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          NETLISTS / EXPORT                               --
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
--   ToDo: 


with ada.text_io;				use ada.text_io;

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.directories;
with gnat.directory_operations;
with ada.exceptions;

with et_net_names;				use et_net_names;
with et_netchangers.schematic;	use et_netchangers.schematic;

with et_port_direction;			use et_port_direction;
with et_port_names;				use et_port_names;
with et_terminal_name;			use et_terminal_name;
with et_device_name;			use et_device_name;
with et_net_ports_netchangers;	use et_net_ports_netchangers;
with et_netlist_name;			use et_netlist_name;

with et_export;
with et_generic_stacks;
with et_csv;					use et_csv;
with et_time;					use et_time;
with et_system_info;

with et_string_processing;		use et_string_processing;



package body et_netlists_export is

	use pac_net_name;
	

	

	
	
	procedure write_netlist (
		netlist			: in pac_module_netlist.tree;
		module_name		: in pac_module_name.bounded_string; -- motor_driver 
		variant_name	: in pac_assembly_variant_name.bounded_string; -- low_cost
		log_threshold	: in type_log_level) 
	is

		file_name : pac_netlist_file_name.bounded_string;

		
		procedure set_file_name is 
			use ada.directories;
			use gnat.directory_operations;
			use pac_assembly_variant_name;
			use et_export;
		begin
			if is_default (variant_name) then
				file_name := to_file_name (
							compose 
							(
								containing_directory	=> directory_export & dir_separator & directory_cam &
															dir_separator & directory_netlists,

								name					=> to_string (module_name),
								extension				=> extension_netlist
							));

			else
				file_name := to_file_name (
							compose 
							(
								containing_directory	=> directory_export & dir_separator & directory_cam &
															dir_separator & directory_netlists,

								name					=> to_string (module_name) & "_" & 
															to_variant (variant_name),
								extension				=> extension_netlist
							));
			end if;
		end;	

		
		netlist_handle : ada.text_io.file_type;

		
		-- writes a nice header in the netlist file
		procedure write_header is 
			use pac_module_netlist;
			use et_system_info;
			netlist_cursor : pac_module_netlist.cursor := root (netlist);
		begin
			put_line (netlist_handle, comment_mark & " " & system_name & " " & version & " netlist");
			put_line (netlist_handle, comment_mark & " " & get_date);
			put_line (netlist_handle, comment_mark & " module " & enclose_in_quotes (to_string (module_name)));
			put_line (netlist_handle, comment_mark & " " & row_separator_double);
			put_line (netlist_handle, comment_mark & " net count total" & count_type'image (child_count (netlist_cursor)));
			-- CS: use dedicated subtype for net count instead of count_type
			-- CS: statistics about pin count ?
			
			put_line (netlist_handle, comment_mark);
			put_line (netlist_handle, comment_mark & " legend:");
			put_line (netlist_handle, comment_mark & "  net_name");
			put_line (netlist_handle, comment_mark & "  device port direction terminal/pin/pad");
			put_line (netlist_handle, comment_mark);
			put_line (netlist_handle, comment_mark & "  Names of secondary nets are comments.");
			put_line (netlist_handle, comment_mark & " " & row_separator_single);
		end write_header;

		
		-- writes a nice footer in the netlist file
		procedure write_footer is begin
			new_line (netlist_handle);
			put_line (netlist_handle, comment_mark & " " & row_separator_double);
			put_line (netlist_handle, comment_mark & " end of list");
		end write_footer;


		
		procedure write_nets is 
			use pac_module_netlist;

			
			-- Writes the device port in the netlist file.
			procedure query_device (port_cursor : in pac_device_ports_extended.cursor) is
				use pac_device_ports_extended;
			begin
				put_line (netlist_handle, -- IC1 CE input H5
					to_string (element (port_cursor).device) & latin_1.space &
					to_string (element (port_cursor).port) & latin_1.space &
					to_string (element (port_cursor).direction) & latin_1.space &
					to_string (element (port_cursor).terminal) & latin_1.space);
					-- CS .characteristics
			end;

			
			procedure query_secondary_net (net_cursor : in pac_module_netlist.cursor) is begin
				-- Extract the ports of devices of the secondary net:

				log_indentation_up;
				log_net_name (element (net_cursor).name, false, log_threshold + 1);

				-- write the secondary net name as comment:
				new_line (netlist_handle);
				put_line (netlist_handle, comment_mark & latin_1.space &
					to_string (element (net_cursor).name.prefix) & 
					to_string (element (net_cursor).name.base_name)); -- CLK_GENERATOR/FLT1/ & clock_out
				
				pac_device_ports_extended.iterate (element (net_cursor).devices, query_device'access);

				-- Extract the ports of netchangers:
				null; -- CS
				
				-- Iterate secondary nets:
				iterate_children (net_cursor, query_secondary_net'access);

				log_indentation_down;
			end;

			
			procedure write_primary_net (net_cursor : in pac_module_netlist.cursor) is begin
				log_indentation_up;
				log_net_name (element (net_cursor).name, true, log_threshold + 1);
				
				-- write the primary net name:
				new_line (netlist_handle);
				put_line (netlist_handle, comment_mark & " -------");
				put_line (netlist_handle, to_string (element (net_cursor).name.prefix) & 
					to_string (element (net_cursor).name.base_name)); -- CLK_GENERATOR/FLT1/ & clock_out

				-- Extract the ports of devices of the primary net:
				pac_device_ports_extended.iterate (element (net_cursor).devices, query_device'access);

				-- Extract the ports of netchangers:
				null; -- CS
				
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
		modules			: in pac_netlist_modules.tree;
		module_name		: in pac_module_name.bounded_string; -- motor_driver 
		variant_name	: in pac_assembly_variant_name.bounded_string; -- low_cost
		write_file		: in boolean;
		log_threshold	: in type_log_level)
		return pac_module_netlist.tree
	is
		use pac_netlist_modules;
		
		use pac_module_netlist;
		netlist : pac_module_netlist.tree; -- to be returned

		
		-- While exploring the tree of modules (with their individual netlists) the
		-- cursor in container "netlist" points to a certain net. This cursor must be 
		-- backup on a stack (see below) each time we dive into secondary nets.
		netlist_cursor : pac_module_netlist.cursor := pac_module_netlist.root (netlist);

		
		-- When exploring secondary nets, the cursor to a net must be backup.
		-- This must be done each time a secondary net is discovered.
		package stack is new et_generic_stacks.stack_lifo (
			item	=> pac_module_netlist.cursor,
			max 	=> nesting_depth_max);

		
		procedure find_dependencies ( -- prespecification only. see body below.
			module_cursor	: in pac_netlist_modules.cursor;
			net_cursor		: in pac_nets.cursor;
			log_threshold	: in type_log_level);

		
		-- Explores global secondary nets of given net in submodules of given module.
		-- If the given module does not have submodules, nothing happens.
		-- Calls find_dependencies.
		procedure query_global_nets_in_submodules (
			module_cursor	: in pac_netlist_modules.cursor;
			net_cursor		: in pac_nets.cursor;
			log_threshold	: in type_log_level) 
		is

			
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

					pac_module_netlist.insert_child (
						container	=> netlist,
						parent		=> netlist_cursor,
						before		=> pac_module_netlist.no_element,
						position	=> netlist_cursor,
						new_item	=> (element (glob_net.net) with key (glob_net.net)));
					
					-- CS: Make sure the net is not connected via a port with the parent module.
-- 					if not contains (
-- 						net_cursor		=> net_cursor,
-- 						submodule		=> element (glob_net.submodule).instance_name, -- OSC1
-- 						port			=> key (glob_net.net).base_name) then -- clock_out

					log (text => "submodule " &
						enclose_in_quotes (to_string (pac_netlist_modules.element (glob_net.submodule).generic_name)) &
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


		
		
		-- Explores secondary nets starting with the given net in the given module.
		-- NOTE: This procedure is recursive, means it calls itself until all
		--       secondary nets have been found.
		procedure find_dependencies (
			module_cursor	: in pac_netlist_modules.cursor; -- the module we are in
			net_cursor		: in pac_nets.cursor; -- the net we are in
			log_threshold	: in type_log_level) 
		is
			
			use pac_nets;
			
			-- In order to save computing time, get number of netchanger and submodule 
			-- ports of this net before exploring the net.
			ports : type_port_count := get_port_count (net_cursor);
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
						
						pac_module_netlist.insert_child (
							container	=> netlist,
							parent		=> netlist_cursor,
							before		=> pac_module_netlist.no_element,
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
						
						pac_module_netlist.insert_child (
							container	=> netlist,
							parent		=> netlist_cursor,
							before		=> pac_module_netlist.no_element,
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

					pac_module_netlist.insert_child (
						container	=> netlist,
						parent		=> netlist_cursor,
						before		=> pac_module_netlist.no_element,
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
			
			procedure query_nets (module_cursor : in pac_netlist_modules.cursor) is 

				
				procedure query_ports (net_cursor : in pac_nets.cursor) is
					use pac_nets;
				begin
					-- Extract primary nets only:
					if is_primary (net_cursor) then

						log_indentation_up;

						log_net_name (key (net_cursor), true, log_threshold + 1);

						-- backup netlist cursor before diving into secondary nets
						stack.push (netlist_cursor);
						
						pac_module_netlist.insert_child (
							container	=> netlist,
							parent		=> root (netlist),
							before		=> pac_module_netlist.no_element,
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

		
	begin
		explore_nets;

		if write_file then
			write_netlist (netlist, module_name, variant_name, log_threshold);
		end if;
		
		return netlist;
				
	end make_netlist;

	

	
end et_netlists_export;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
