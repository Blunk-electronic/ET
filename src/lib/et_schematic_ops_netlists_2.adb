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

with et_module_names;
with et_module;

with et_net_names;
with et_nets;
with et_net_ports;

with et_devices_electrical;

-- with et_netlists_export;

with et_device_library.units;
with et_schematic_ops_units;
with et_schematic_ops_device;

with et_assembly_variants;
with et_schematic_ops_assembly_variant;

with et_netlist_cat_1;


package body et_schematic_ops_netlists_2 is



	function to_string (
		category	: in type_netlist_category)
		return string
	is begin
		return type_netlist_category'image (category);
	end;



	
	function to_netlist_category (
		category	: in string)
		return type_netlist_category
	is
		result : type_netlist_category := type_netlist_category'first;
	begin
		if category = "1" then
			result := NETLIST_CAT_1;
		elsif category = "2" then
			result := NETLIST_CAT_2;
		end if;
		
		return result;
	end;

	
	
	
	
	
	
	function extend_ports (
		module_cursor	: in pac_generic_modules.cursor;
		ports 			: in pac_device_ports.set)
		return pac_device_ports_extended.set 
	is
		ports_extended : pac_device_ports_extended.set; -- to be returned

		use pac_device_ports;

		
		procedure query_ports (
			port_cursor : in pac_device_ports.cursor) 
		is
			port_sch		: type_device_port := element (port_cursor);
			
			use et_device_library.units;
			more_properties	: type_port_properties_access;
			
			use et_devices_electrical;
			device_cursor	: pac_devices_electrical.cursor;

			use et_schematic_ops_device;
			use et_schematic_ops_units;
			
		begin
			device_cursor := get_electrical_device (
				module_cursor, element (port_cursor).device_name);

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
					new_item	=> (
						direction		=> more_properties.direction, -- CS
						device			=> port_sch.device_name, -- IC1
						port			=> port_sch.port_name, -- CE
						terminal		=> more_properties.terminal,
						characteristics => more_properties.properties));
			end if;
		end query_ports;

		
	begin
		iterate (ports, query_ports'access);
		return ports_extended;
	end extend_ports;

	
	
	
	
	
	

	function extend_ports (
		module_cursor	: in pac_generic_modules.cursor;
		ports 			: in pac_net_submodule_ports.set)
		return pac_submodule_ports_extended.set 
	is
		ports_extended : pac_submodule_ports_extended.set; -- to be returned

		use pac_net_submodule_ports;

		
		procedure query_ports (
			port_cursor : in pac_net_submodule_ports.cursor) 
		is
			port : type_net_submodule_port renames element (port_cursor);
			-- direction : type_netchanger_port_name; -- master/slave
		begin
 			-- get the direction of the current submodule port
			-- direction := port_direction (
				-- module_cursor, port.module_name, port.port_name);

			pac_submodule_ports_extended.insert (
				container	=> ports_extended,
				new_item	=> (
					submodule	=> port.module_name, -- OSC1
					port		=> port.port_name)); -- clock_out
					-- CS direction, characteristics ?
			
		end query_ports;

		
	begin
		iterate (ports, query_ports'access);
		return ports_extended;
	end extend_ports;
	
	
	
	
	
	

	 
	
	
	
	
	procedure make_netlist_cat_1 (
		module_cursor 	: in pac_generic_modules.cursor;
		variant			: in pac_assembly_variant_name.bounded_string;
		log_threshold	: in type_log_level) 
	is
		use et_module;
		use et_module_names;
		use et_netlist_cat_1;

		use et_assembly_variants;
		variant_cursor : pac_assembly_variants.cursor;
		
		-- The netlist to be generated:
		netlist : pac_netlist_cat_1.map;
		
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			use et_nets;
			use pac_nets;
			
			use et_net_names;
			use pac_net_name;
			net_cursor : pac_nets.cursor := module.nets.first;
			
			
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in type_net)
			is
				use et_net_ports;
				all_ports : type_net_ports;
				
				use et_netlist_devices;
				device_ports_extended : pac_device_ports_extended.set;
				
				use et_netlist_submodules;
				submodule_ports_extended : pac_submodule_ports_extended.set;
				
			begin
				log (text => "net " & to_string (net_name),
					level => log_threshold + 1);
					
				log_indentation_up;

				-- Get all device, netchanger and submodule ports of this net
				-- according to the given assembly variant:
				all_ports := get_net_ports (net_cursor, variant_cursor);

				-- Extend the submodule ports with more properties:
				submodule_ports_extended := extend_ports (
					module_cursor, all_ports.submodules);
				
				-- Extend the device ports with more properties:
				device_ports_extended := extend_ports (
					module_cursor, all_ports.devices);

				-- Add the net to the netlist that is to
				-- be generated:
				add_net_to_netlist (
					netlist		=> netlist,
					name		=> net_name, 
					devices		=> device_ports_extended,
					submodules	=> submodule_ports_extended,
					netchangers	=> all_ports.netchangers);
				
				log_indentation_down;
			end query_net;
			
			
		begin
			-- Iterate through the nets of the module:
			while has_element (net_cursor) loop		
				query_element (net_cursor, query_net'access);
				next (net_cursor);
			end loop;
		end query_module;
		
			
			
		use et_schematic_ops_assembly_variant;
			
	begin
		log (text => "module " & to_string (module_cursor)
			& " assembly variant " & to_variant (variant)
			& " generate netlist " & to_string (NETLIST_CAT_1),
			level => log_threshold);
		
		log_indentation_up;

		variant_cursor := get_assembly_variant (
			module_cursor, variant);
				
		query_element (module_cursor, query_module'access);
		
		log_indentation_down;
	end make_netlist_cat_1;


	
end et_schematic_ops_netlists_2;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
