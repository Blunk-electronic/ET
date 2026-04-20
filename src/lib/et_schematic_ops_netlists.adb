------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                   SCHEMATIC OPERATIONS / NETLISTS                        --
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

--   For correct displaying set tab with in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.exceptions;					use ada.exceptions;

with et_module;							use et_module;
with et_module_instance;				use et_module_instance;
with et_nets;							use et_nets;

with et_netlists_export;

with et_device_library.units;			use et_device_library.units;
with et_schematic_ops_submodules;
with et_schematic_ops_units;
with et_schematic_ops_device;			use et_schematic_ops_device;
with et_generic_stacks;

with et_assembly_variants;				use et_assembly_variants;
with et_schematic_ops_assembly_variant;	use et_schematic_ops_assembly_variant;

with et_submodules;
with et_netchangers.schematic;			use et_netchangers.schematic;

with et_string_processing;				use et_string_processing;




package body et_schematic_ops_netlists is

	use et_netchangers;
	use pac_netchangers;
	
	use et_submodules.pac_submodules;

	
	
	
	
	function extend_ports (
		module_cursor	: in pac_generic_modules.cursor;
		ports 			: in pac_device_ports.set)
		return pac_device_ports_extended.set 
	is
		ports_extended : pac_device_ports_extended.set; -- to be returned

		use pac_device_ports;

		
		procedure query_ports (port_cursor : in pac_device_ports.cursor) is
			port_sch		: type_device_port := element (port_cursor);
			more_properties	: type_port_properties_access;
			device_cursor	: pac_devices_electrical.cursor;

			use et_schematic_ops_units;
		begin
			device_cursor := get_electrical_device (module_cursor, element (port_cursor).device_name);

			-- Get further properties of the current port if the device
			-- is real (appears in PCB):
			if is_real (device_cursor) then
				
				more_properties := get_port_properties (
					module_cursor	=> module_cursor, 
					device_name		=> port_sch.device_name, 
					unit_name		=> port_sch.unit_name,
					port_name		=> port_sch.port_name);
				
				pac_device_ports_extended.insert (
					container	=> ports_extended,
					new_item	=> 
						(
						direction		=> more_properties.direction, -- CS
						device			=> port_sch.device_name, -- IC1
						port			=> port_sch.port_name, -- CE
						terminal		=> more_properties.terminal,
						characteristics => more_properties.properties)
						);
			end if;
		end query_ports;

		
	begin -- extend_ports
		iterate (ports, query_ports'access);
		return ports_extended;
	end extend_ports;

	
	


	
	-- Returns the direction (master/slave) of the given submodule port in module indicated by module_cursor.
	-- The submodule must exist in the module.
	function port_direction (
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		submod_instance	: in pac_module_instance_name.bounded_string; -- OSC1
		port_name		: in pac_net_name.bounded_string) -- clock_out
		return type_netchanger_port_name 
	is
		use et_submodules;
		direction : type_netchanger_port_name; -- to be returned

		
		procedure query_submodules (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			submod_cursor : pac_submodules.cursor;

			
			procedure query_ports (
				submod_name	: in pac_module_instance_name.bounded_string;
				submod		: in et_submodules.type_submodule) 
			is
				use et_submodules.pac_submodule_ports;
				port_cursor : et_submodules.pac_submodule_ports.cursor;
			begin
				port_cursor := find (submod.ports, port_name);

				direction := element (port_cursor).direction;
			end query_ports;

			
		begin -- query_submodules
			-- locate submodule in schematic
			submod_cursor := find (module.submods, submod_instance);

			query_element (submod_cursor, query_ports'access);
		end query_submodules;

		
	begin
		query_element (
			position	=> module_cursor,
			process		=> query_submodules'access);
		
		return direction;
	end port_direction;


	


	

	
	function extend_ports (
		module_cursor	: in pac_generic_modules.cursor;
		ports 			: in pac_net_submodule_ports.set)
		return pac_submodule_ports_extended.set 
	is
		ports_extended : pac_submodule_ports_extended.set; -- to be returned

		use pac_net_submodule_ports;

		procedure query_ports (port_cursor : in pac_net_submodule_ports.cursor) is
			port : type_net_submodule_port := element (port_cursor);
			direction : type_netchanger_port_name; -- master/slave
		begin
 			-- get the direction of the current submodule port
			direction := port_direction (module_cursor, port.module_name, port.port_name);

			pac_submodule_ports_extended.insert (
				container	=> ports_extended,
				new_item	=> 
					(
					module		=> port.module_name, -- OSC1
					port		=> port.port_name, -- clock_out
					direction	=> direction -- master/slave
					)
				);
			
		end query_ports;
		
	begin
		iterate (ports, query_ports'access);
		return ports_extended;
	end extend_ports;






	



	procedure collect_nets (
		module_cursor	: in pac_generic_modules.cursor;
		variant			: in pac_assembly_variant_name.bounded_string;
		prefix			: in pac_net_name.bounded_string; -- DRV3/OSC1/
		offset			: in type_name_index;
		netlist_tree 	: in out pac_netlist_modules.tree;
		netlist_cursor 	: in pac_netlist_modules.cursor;
		log_threshold	: in type_log_level)
	is
		-- CS: rework, simplify code
		
		use et_assembly_variants.pac_assembly_variants;
		variant_cursor : et_assembly_variants.pac_assembly_variants.cursor;

		
		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			use et_nets.pac_nets;
			net_cursor_sch : et_nets.pac_nets.cursor := module.nets.first;

			net_name : pac_net_name.bounded_string;
			all_ports : type_net_ports;
			device_ports_extended : pac_device_ports_extended.set;
			submodule_ports_extended : pac_submodule_ports_extended.set;

			
			-- Applies the given offset to the devices
			-- in device_ports_extended:
			procedure apply_offsets is
				use pac_device_ports_extended;
				-- temporarily the ports will be stored here. Once all ports of
				-- device_ports_extended have been offset, the list
				-- ports_with_offset overwrites device_ports_extended:
				ports_with_offset : pac_device_ports_extended.set;

				
				procedure query_ports (
					cursor : in pac_device_ports_extended.cursor)
				is 
					-- take a copy of the port as it is:
					port : type_device_port_extended := element (cursor);
				begin
					-- apply offset to device name of port
					apply_offset (port.device, offset, log_threshold + 2);

					-- insert the modified port in the container ports_with_offset
					pac_device_ports_extended.insert (
						container	=> ports_with_offset,
						new_item	=> port);
				end;

				
			begin
				iterate (device_ports_extended, query_ports'access);

				-- overwrite by ports_with_offset
				device_ports_extended := ports_with_offset;
			end apply_offsets;

			
			
			procedure insert_net (
				module : in out type_netlist_module)
			is begin
				-- Prepend the given net prefix to the net name.
				-- Insert the net with its ports in the netlist of the submodule.
				et_netlists.pac_netlist_nets.insert (
					container	=> module.nets,
					key			=> (prefix => prefix, base_name => net_name), -- CLK_GENERATOR/FLT1/ , clock_out
					new_item	=> (
							devices		=> device_ports_extended,
							submodules	=> submodule_ports_extended,
							netchangers	=> all_ports.netchangers,
							scope		=> element (net_cursor_sch).scope)
					--position	=> net_cursor_netlist,
					--inserted	=> inserted
					);
					-- CS: constraint_error arises here if net already in list. should never happen.
			end insert_net;

			
			
		begin -- query_nets			
			if is_default (variant) then
				variant_cursor := et_assembly_variants.pac_assembly_variants.no_element;
			else
				variant_cursor := find (module.assembly_variants.variants, variant);
				if variant_cursor = et_assembly_variants.pac_assembly_variants.no_element then
					assembly_variant_not_found (variant);
				end if;
			end if;
			-- Now variant_cursor points to the given assembly variant. If it points to
			-- no element then it is about the default variant.
			
			-- loop in nets of given module
			while has_element (net_cursor_sch) loop

				net_name := et_nets.pac_nets.key (net_cursor_sch);
				
				log (text => "net " 
						& pac_net_name.to_string (prefix) 
						& pac_net_name.to_string (net_name),
						level => log_threshold + 1);

				log_indentation_up;
				
				-- Get all device, netchanger and submodule ports of this net
				-- according to the given assembly variant:
				all_ports := get_ports (net_cursor_sch, variant_cursor);
			
				-- extend the submodule ports by their directions (master/slave):
				submodule_ports_extended := extend_ports (module_cursor, all_ports.submodules);
				
				-- extend the device ports by further properties (direction, terminal name, ...):
				device_ports_extended := extend_ports (module_cursor, all_ports.devices);
				
				-- The portlist device_ports_extended now requires the device indexes 
				-- to be changed according to the given offset:
				apply_offsets;
				
				-- insert the net with its ports in the list of nets
				pac_netlist_modules.update_element (
					container	=> netlist_tree,
					position	=> netlist_cursor,
					process		=> insert_net'access);
				
				log_indentation_down;
				
				next (net_cursor_sch);
			end loop;
		end query_nets;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " collect nets", level => log_threshold);
			
		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_nets'access);

		log_indentation_down;
	end collect_nets;



	


	
	
	function make_prefix (
		tree_cursor		: in pac_renumber_modules.cursor)
		return pac_net_name.bounded_string
	is
		use pac_net_name;
		prefix : pac_net_name.bounded_string;
		
		use pac_renumber_modules;
		cursor : pac_renumber_modules.cursor := tree_cursor;
	begin
		-- The first prefix to PREPEND is the name of the current submodule instance:
		prefix := to_prefix (element (cursor).instance);

		-- look for the overlying parent submodule
		cursor := parent (cursor);

		-- travel upwards toward other overlying submodules. The search ends as
		-- soon as the top module has been reached.
		while not is_root (cursor) loop
			-- prepend instance name of parent submodule
			prefix := to_prefix (element (cursor).instance) & prefix;
			cursor := parent (cursor);
		end loop;
			
		return prefix;
	end make_prefix;

	

	
	
		
	
	
	
	-- This stack keeps record of the netlist_cursor as we 
	-- go trough the design structure:
	package stack_netlist is new et_generic_stacks.stack_lifo (
		item	=> pac_netlist_modules.cursor,
		max 	=> et_submodules.nesting_depth_max);

	
	-- A stack keeps record of the submodule level where 
	-- tree_cursor is pointing at.
	package stack_level is new et_generic_stacks.stack_lifo (
		item	=> pac_renumber_modules.cursor,
		max 	=> et_submodules.nesting_depth_max);

	
	-- Another stack keeps record of the assembly variant 
	-- at the submodule level.
	package stack_variant is new et_generic_stacks.stack_lifo (
		item	=> pac_assembly_variant_name.bounded_string,
		max 	=> et_submodules.nesting_depth_max);

	
	
	
	procedure query_submodules (
		module_cursor	: in pac_generic_modules.cursor;
		variant_name	: in pac_assembly_variant_name.bounded_string;
		netlist_tree 	: in out pac_netlist_modules.tree;
		netlist_cursor 	: in out pac_netlist_modules.cursor;
		variant			: in out pac_assembly_variant_name.bounded_string;
		log_threshold	: in type_log_level)	
	is 
				
		
		-- This procedure queries the given top module
		-- and iterates through its submodules:
		procedure query_topmodule (
			top_module_name	: in pac_module_name.bounded_string;
			top_module		: in type_generic_module)
		is
			submodule_name 	: pac_module_name.bounded_string;
			
			parent_name : pac_module_name.bounded_string; -- water_pump
			
			submodule_instance	: pac_module_instance_name.bounded_string; -- MOT_DRV_3
			
			offset : type_name_index;

			alt_submod : pac_submodule_variants.cursor;
						
			
			-- Get the root of the submodules tree:
			tree_cursor : pac_renumber_modules.cursor := 
				top_module.submod_tree.root;
			
			
			-- Insert a submodule in netlist_tree. 
			-- Wherever procedure query_submodules is
			-- called, cursor netlist_cursor is pointing 
			-- at the latest parent module. The submodules 
			-- detected here must be inserted as children 
			-- of that parent module:
			procedure insert_submodule is begin

				-- backup netlist_cursor
				stack_netlist.push (netlist_cursor);
				
				pac_netlist_modules.insert_child (
					container	=> netlist_tree,
					parent		=> netlist_cursor,
					before		=> pac_netlist_modules.no_element,
					position	=> netlist_cursor, -- points afterwards to the child that has just been inserted
					new_item	=> (
						generic_name	=> submodule_name,
						instance_name	=> submodule_instance,
						others			=> <>)
					);

				-- Collect nets from current module. inserts the nets in
				-- the submodule indicated by netlist_cursor:
				collect_nets (
					module_cursor	=> locate_module (submodule_name),
					variant			=> variant,
					prefix			=> make_prefix (tree_cursor),
					offset			=> offset,
					netlist_tree	=> netlist_tree,
					netlist_cursor	=> netlist_cursor,
					log_threshold	=> log_threshold + 2);
				
				-- restore netlist_cursor
				netlist_cursor := stack_netlist.pop;
			end insert_submodule;
		

			use et_schematic_ops_submodules;
			use pac_submodule_variants;
			use pac_renumber_modules;
		
		begin			
			-- Start with the first submodule on the 
			-- current hierarchy level:
			tree_cursor := first_child (tree_cursor);

			
			-- iterate through the submodules on this level
			while has_element (tree_cursor) loop
			
				-- Get the generic name of the submodule candidate:
				submodule_name := element (tree_cursor).name;
				
				-- Get the instance name of the submodule candidate:
				submodule_instance := element (tree_cursor).instance;
				
				log (text => "submodule instance " 
					& enclose_in_quotes (to_string (submodule_instance)) 
					& " of generic module " & enclose_in_quotes (to_string (submodule_name)),
					level => log_threshold + 1);

				-- In case we are on the first level, the parent module is the given top module.
				-- In that case the parent variant is the given variant of the top module.
				-- If the top module has the default variant, all submodules in all levels
				-- assume default variant too.
				if parent (tree_cursor) = root (top_module.submod_tree) then
					parent_name := top_module_name;
					variant := variant_name; -- argument of make_for_variant
				else
					parent_name := element (parent (tree_cursor)).name;
				end if;

				
				-- Get the device name offset of the current submodule;
				offset := element (tree_cursor).device_names_offset;
				
				
				if not is_default (variant) then
					-- Query in parent module: Is there any assembly variant specified for this submodule ?

					alt_submod := get_alternative_submodule (
								module	=> locate_module (parent_name),
								variant	=> variant,
								submod	=> submodule_instance);

					if alt_submod = et_assembly_variants.pac_submodule_variants.no_element then
					-- no variant specified for this submodule -> collect devices of default variant

						variant := default;
					else
					-- alternative variant specified for this submodule
						variant := element (alt_submod).variant;
					end if;

				end if;

				
				-- Insert submodule in netlist_tree.
				insert_submodule;

				
				if first_child (tree_cursor) = pac_renumber_modules.no_element then 
				-- No submodules on the current level. means we can't go deeper:
					
					log_indentation_up;
					log (text => "no submodules here -> bottom reached",
						level => log_threshold + 1);
					log_indentation_down;
				else
				-- There are submodules on the current level:
					
					-- backup the cursor to the current submodule on this level
					stack_level.push (tree_cursor);

					-- backup the parent assembly variant
					stack_variant.push (variant);

					-- iterate through submodules on the level below
					query_submodules ( -- this is recursive !
						module_cursor	=> module_cursor,
						variant_name	=> variant_name,
						netlist_tree	=> netlist_tree,
						netlist_cursor	=> netlist_cursor,
						-- submod_tree		=> submod_tree,
						-- tree_cursor		=> tree_cursor,
						variant			=> variant,
						log_threshold	=> log_threshold);

					-- restore cursor to submodule (see stack_level.push above)
					tree_cursor := stack_level.pop;

					-- restore the parent assembly variant (see stack_variant.push above)
					variant := stack_variant.pop;
				end if;

				next_sibling (tree_cursor); -- next submodule on this level
			end loop;
		
		end query_topmodule;
		
		
		
	begin
		log (text => "top-module " & to_string (module_cursor)
			& " query submodules", level => log_threshold);		

		log_indentation_up;
		
		query_element (module_cursor, query_topmodule'access);
		
		log_indentation_down;

		-- CS: rework exception handler
		exception
			when event: others =>
				log_indentation_reset;
				log (text => ada.exceptions.exception_information (event), console => true);
				raise;
		
	end query_submodules;


	
	
	
	
	
	
	procedure make_netlists (
		module_cursor 	: in pac_generic_modules.cursor;
		write_files		: in boolean := false;
		log_threshold	: in type_log_level) 
	is
		use et_netlists_export;

		use et_assembly_variants;
		use et_assembly_variants.pac_assembly_variants;
		use pac_assembly_variant_name;

		
		procedure make_for_variant (
			variant_name : in pac_assembly_variant_name.bounded_string)
		is
			-- Since we are dealing with hierarchic designs, a tree of modules (each of them having its
			-- own netlist) is required. In the course of this procedure the netlist_tree is built
			-- and finally passed to netlists.write_netlist for further processing.
			-- The netlist_tree does not provide information on dependencies between nets (such
			-- as primary or secondary nets. see netlist specs).
			netlist_tree : pac_netlist_modules.tree := pac_netlist_modules.empty_tree;
			netlist_cursor : pac_netlist_modules.cursor := pac_netlist_modules.root (netlist_tree);


			variant : pac_assembly_variant_name.bounded_string; -- low_cost
			
			-- before updating the netlist of the module we keep the new netlist here temporarily:
			netlist : pac_module_netlist.tree;

			
			-- Updates the netlist of the module. The netlist is indicated by the variant_name.
			procedure update_netlist (
				module_name		: in pac_module_name.bounded_string;
				module			: in out type_generic_module) 
			is
				
				procedure assign_netlist (
					variant		: in pac_assembly_variant_name.bounded_string;
					netlist		: in out pac_module_netlist.tree) 
				is begin
					-- overwrite the current netlist by the new netlist:
					netlist := make_for_variant.netlist;
				end assign_netlist;

				use pac_module_netlists;
				netlist_cursor : pac_module_netlists.cursor;

				
			begin -- update_netlist
				log (text => "updating netlist ...", level => log_threshold + 2);
				
				-- Locate the netlist within the module.
				-- If the netlist does not exist yet, insert it in module.netlists.
				-- If the netlist does exist, overwrite it by the new netlist.
				netlist_cursor := find (module.netlists, variant_name);

				if netlist_cursor = pac_module_netlists.no_element then

					pac_module_netlists.insert (
						container	=> module.netlists,
						key			=> variant_name,
						new_item	=> make_for_variant.netlist); -- the new netlist

				else
					pac_module_netlists.update_element (
						container	=> module.netlists,
						position	=> netlist_cursor,
						process		=> assign_netlist'access);

				end if;
			end update_netlist;

			
		begin -- make_for_variant
			if is_default (variant_name) then
				log (text => "default assembly variant ", level => log_threshold + 1);
			else
				log (text => "assembly variant " &
					enclose_in_quotes (to_string (variant_name)), level => log_threshold + 1);
			end if;

			log_indentation_up;

			stack_level.init;
			stack_variant.init;
			stack_netlist.init;

			-- Insert the top module in the netlist_tree. It is the only node on this level.
			-- Submodules will be inserted as children of the top module (where netlist_cursor 
			-- points at AFTER this statement):
			pac_netlist_modules.insert_child (
				container	=> netlist_tree,
				parent		=> pac_netlist_modules.root (netlist_tree),
				before		=> pac_netlist_modules.no_element,
				position	=> netlist_cursor,
				new_item	=> (
					generic_name	=> key (make_netlists.module_cursor),
					instance_name	=> to_instance_name (""), -- the top module has no instance name
					others 			=> <>)
				);
			-- netlist_cursor now points at the top module in netlist_tree.

			-- Collect nets of the given top module. the top module has no device index offset.
			-- The nets will be inserted where netlist_cursor points at.
			collect_nets (
				module_cursor	=> module_cursor,
				variant			=> variant_name,
				prefix			=> to_net_name (""), -- no net prefix in top module
				offset			=> 0,
				netlist_tree	=> netlist_tree,
				netlist_cursor	=> netlist_cursor,
				log_threshold	=> log_threshold + 2);

			-- collect devices of the submodules
			query_submodules (
				module_cursor	=> module_cursor,
				variant_name	=> variant_name,
				netlist_tree	=> netlist_tree,
				netlist_cursor	=> netlist_cursor,
				variant			=> variant,
				log_threshold	=> log_threshold + 2);

			
			
			-- Container netlist_tree is now ready for further processing.
			-- It contains the modules and their nets ordered in a tree structure.
			-- But the connections between nets are
			-- still unknown and will be analyzed now:
			netlist := make_netlist (
				modules			=> netlist_tree,	
				module_name		=> key (module_cursor), -- motor_driver (to be written in the netlist file header)
				variant_name	=> variant_name, 	-- low_cost, empty if default variant
				write_file		=> write_files,
				log_threshold	=> log_threshold);

			-- Now netlist provides information on primary nets and their subordinated secondary nets.
			
			-- Update the netlist (indicated by variant_name) in the module by variable "netlist".
			-- NOTE: This is about the internal netlist (module.netlists) and has nothing to do
			-- with netlist files to be exported for manufacturing and testing:
			update_element (
				container		=> generic_modules,
				position		=> module_cursor,
				process			=> update_netlist'access);
			
			log_indentation_down;
		end make_for_variant;

		
		procedure query_variant (variant_cursor : in et_assembly_variants.pac_assembly_variants.cursor) is
			use pac_assembly_variant_name;
		begin
			make_for_variant (key (variant_cursor));
		end query_variant;

		
	begin -- make_netlists
		log (text => "module " & to_string (module_cursor)
			 & " generate netlists", level => log_threshold);
		
		log_indentation_up;

		-- Build the submodule tree of the module according 
		-- to the current design structure.
		-- All further operations rely on this tree:
		et_schematic_ops_submodules.build_submodules_tree (
			module_cursor 	=> module_cursor,
			log_threshold	=> log_threshold + 1);

		-- make netlist of default variant
		make_for_variant (default);

		-- make netlists of other variants
		et_assembly_variants.pac_assembly_variants.iterate (
			element (module_cursor).assembly_variants.variants, query_variant'access);
		
		log_indentation_down;
	end make_netlists;


	
end et_schematic_ops_netlists;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
