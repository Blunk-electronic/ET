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

with et_module_names;					use et_module_names;
with et_module;							use et_module;
-- with et_module_instance;				use et_module_instance;
with et_nets;							use et_nets;
with et_net_ports;						use et_net_ports;
-- with et_netlists_export;
-- 
-- with et_device_library.units;			use et_device_library.units;
-- with et_schematic_ops_submodules;
-- with et_schematic_ops_units;
-- with et_schematic_ops_device;			use et_schematic_ops_device;
-- with et_generic_stacks;

with et_assembly_variants;				use et_assembly_variants;
with et_schematic_ops_assembly_variant;	use et_schematic_ops_assembly_variant;

-- with et_submodules;
-- with et_netchangers.schematic;			use et_netchangers.schematic;

-- with et_string_processing;				use et_string_processing;




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

	
	
	
	
	procedure make_netlist (
		module_cursor 	: in pac_generic_modules.cursor;
		variant			: in pac_assembly_variant_name.bounded_string;
		category		: in type_netlist_category;
		log_threshold	: in type_log_level) 
	is
		use et_netlist_cat_1;
		
		variant_cursor : pac_assembly_variants.cursor;
		
		
		-- CS netlist : et_netlist_cat_1.pac_netlist_cat_1.map;
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			use pac_nets;
			use pac_net_name;
			net_cursor : pac_nets.cursor := module.nets.first;
			
			
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in type_net)
			is
				all_ports : type_net_ports;
				-- device_ports_extended : pac_device_ports_extended.set;
				-- submodule_ports_extended : pac_submodule_ports_extended.set;
			begin
				log (text => "net " & to_string (net_name),
					level => log_threshold + 1);
					
				log_indentation_up;

				-- Get all device, netchanger and submodule ports of this net
				-- according to the given assembly variant:
				all_ports := get_ports (net_cursor, variant_cursor);

				
				log_indentation_down;
			end query_net;
			
			
		begin
			-- Iterate through the nets of the module:
			while has_element (net_cursor) loop		
				query_element (net_cursor, query_net'access);
				next (net_cursor);
			end loop;
		end query_module;
		
			
	begin
		log (text => "module " & to_string (module_cursor)
			& " assembly variant " & to_variant (variant)
			& " generate netlist " & to_string (category),
			level => log_threshold);
		
		log_indentation_up;

		variant_cursor := get_assembly_variant (module_cursor, variant);
				
		query_element (module_cursor, query_module'access);
		
		log_indentation_down;
	end make_netlist;


	
end et_schematic_ops_netlists_2;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
