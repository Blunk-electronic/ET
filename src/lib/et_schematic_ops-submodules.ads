------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--          SCHEMATIC OPERATIONS ON NETCHANGERS AND SUBMODULES              --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                -- 
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


package et_schematic_ops.submodules is

	procedure netchanger_not_found (
		index : in et_submodules.type_netchanger_id);

	
	procedure submodule_not_found (
		name : in pac_module_instance_name.bounded_string);	


	procedure port_not_at_edge (
		name : in pac_net_name.bounded_string);

	
	-- Returns true if given port of netchanger is connected with any net.
	function port_connected (
		module	: in pac_generic_modules.cursor;	
		port	: in et_netlists.type_port_netchanger)
		return boolean;

	
	-- Returns true if the given net provides a netchanger that may serve as port
	-- to a parent module.
	function netchanger_as_port_available (
		module		: in pac_generic_modules.cursor;
		net			: in et_nets.pac_nets.cursor;
		direction	: in et_submodules.type_netchanger_port_name) -- master/slave 		
		return boolean;


	-- Returns true if the given module provides the given port.
	-- The module being searched in must be in the rig already.						
	function submodule_port_exists (
		module			: in et_submodules.pac_submodules.cursor;
		port			: in pac_net_name.bounded_string;
		direction		: in et_submodules.type_netchanger_port_name) -- master/slave		
		return boolean;



	-- Returns true if given submodule with the given port exists in module indicated by module_cursor.
	function submodule_port_exists (
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		submod_instance : in pac_module_instance_name.bounded_string; -- MOT_DRV_3
		port_name		: in pac_net_name.bounded_string) -- RESET
		return boolean;


	

	-- Returns the sheet/x/y position of the given submodule port.
	function get_submodule_port_position (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		submod_name		: in pac_module_instance_name.bounded_string; -- MOT_DRV_3
		port_name		: in pac_net_name.bounded_string; -- RESET
		log_threshold	: in type_log_level)
		return et_coordinates_2.type_position;

	
	

	-- Adds a port to a submodule instance (the box in the parent sheet).
	procedure add_port (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		instance		: in pac_module_instance_name.bounded_string; -- OSC1
		port_name		: in pac_net_name.bounded_string; -- clk_out
		position		: in type_vector_model; -- x/y along the edge of the box

		direction		: in et_submodules.type_netchanger_port_name; -- master/slave. 
		-- NOTE: has nothing to do with direction of energy flow. It is relevant when 
		-- a netlist is exported. See specification et_submodules.type_submodule_port.
		
		log_threshold	: in type_log_level);



	-- Deletes a port of a submodule instance (the box in the parent sheet).
	procedure delete_port (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		instance		: in pac_module_instance_name.bounded_string; -- OSC1
		port_name		: in pac_net_name.bounded_string; -- clk_out
		log_threshold	: in type_log_level);


	

	-- Moves the given submodule port. Disconnects the port from
	-- start or end points of net segments BEFORE the move. 
	-- Connects submodule port with segment end or start points AFTER the move.
	procedure move_port (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		instance		: in pac_module_instance_name.bounded_string; -- OSC
		port_name		: in pac_net_name.bounded_string; -- clock_output
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_vector_model; -- x/y
		log_threshold	: in type_log_level);




	-- Drags the given submodule port along the edge of the box.
	-- Already existing connections with net segments are kept.
	-- Net segment positions are modified.
	-- This operation applies to a single sheet. Dragging from one sheet
	-- to another is not possible.
	procedure drag_port (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		instance		: in pac_module_instance_name.bounded_string; -- OSC
		port_name		: in pac_net_name.bounded_string; -- clock_output
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_vector_model; -- x/y
		log_threshold	: in type_log_level);


	
	-- Returns true if given netchanger exists in module indicated by module_cursor.
	function exists_netchanger (
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		index			: in et_submodules.type_netchanger_id) -- 1, 2, 3, ...
		return boolean;


	
	-- Returns the sheet/x/y position of the given netchanger port.
	function get_netchanger_port_position (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in et_submodules.type_netchanger_id; -- 1,2,3,...
		port			: in et_submodules.type_netchanger_port_name; -- SLAVE/MASTER
		log_threshold	: in type_log_level)
		return et_coordinates_2.type_position;

	

	-- Adds a netchanger to the schematic.
	procedure add_netchanger (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		place			: in et_coordinates_2.type_position; -- sheet/x/y
		log_threshold	: in type_log_level);
	

	-- Drags the given netchanger within the schematic.
	-- Already existing connections with net segments are kept.
	-- Net segment positions are modified.
	-- This operation applies to a single sheet. Dragging from one sheet
	-- to another is not possible.
	procedure drag_netchanger (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in et_submodules.type_netchanger_id; -- 1,2,3,...
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_vector_model; -- x/y
		log_threshold	: in type_log_level);



	-- Moves the given netchanger. Disconnects the netchanger from
	-- start or end points of net segments BEFORE the move. 
	-- Connects netchanger ports with segment end or strart points AFTER the move.
	procedure move_netchanger (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in et_submodules.type_netchanger_id; -- 1,2,3,...
		coordinates		: in type_coordinates; -- relative/absolute
		sheet			: in type_sheet_relative; -- -3/0/2
		point			: in type_vector_model; -- x/y
		log_threshold	: in type_log_level);



	-- Rotates the given netchanger. Disconnects it from
	-- start or end points of net segments.
	procedure rotate_netchanger (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in et_submodules.type_netchanger_id; -- 1,2,3,...
		coordinates		: in type_coordinates; -- relative/absolute
		rotation		: in et_coordinates_2.type_rotation_model; -- 90
		log_threshold	: in type_log_level);

	
	
	-- Deletes a netchanger.
	procedure delete_netchanger (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in et_submodules.type_netchanger_id; -- 1,2,3,...
		log_threshold	: in type_log_level);



	-- Adds a submodule instance to the schematic.
	procedure add_submodule (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		file			: in et_submodules.pac_submodule_path.bounded_string; -- the file name of the submodule like templates/oscillator.mod
		instance		: in pac_module_instance_name.bounded_string; -- OSC1
		position		: in et_coordinates_2.type_position; -- sheet, lower left corner x/y 
		size			: in et_submodules.type_submodule_size; -- the size of the box in x and y
		log_threshold	: in type_log_level);

	

	-- Removes a submodule instance from the schematic.
	procedure delete_submodule (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		instance		: in pac_module_instance_name.bounded_string; -- OSC1
		log_threshold	: in type_log_level);

	
	-- Moves the given submodule instance (the box). Disconnects the ports from
	-- start or end points of net segments BEFORE the move. 
	-- Connects submodule ports with segment end or start points AFTER the move.
	procedure move_submodule (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		instance		: in pac_module_instance_name.bounded_string; -- OSC1
		coordinates		: in type_coordinates; -- relative/absolute
		sheet			: in type_sheet_relative; -- -3/0/2
		point			: in type_vector_model; -- x/y
		log_threshold	: in type_log_level);

	
	-- Drags the given submodule instance (the box) within the schematic.
	-- Already existing connections with net segments are kept.
	-- Net segment positions are modified.
	-- This operation applies to a single sheet. Dragging from one sheet
	-- to another is not possible.
	procedure drag_submodule (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		instance		: in pac_module_instance_name.bounded_string; -- OSC1
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_vector_model; -- x/y
		log_threshold	: in type_log_level);

	
	-- Copies a submodule instance.
	procedure copy_submodule (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		instance_origin	: in pac_module_instance_name.bounded_string; -- OSC1
		instance_new	: in pac_module_instance_name.bounded_string; -- CLOCK_GENERATOR
		destination		: in et_coordinates_2.type_position; -- sheet/x/y
		log_threshold	: in type_log_level);

	
	-- Renames a submodule instance.
	procedure rename_submodule (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		instance_old	: in pac_module_instance_name.bounded_string; -- OSC1
		instance_new	: in pac_module_instance_name.bounded_string; -- CLOCK_GENERATOR
		log_threshold	: in type_log_level);



	-- Sets the assembly variant of a submodule instance. An already existing submodule
	-- will be overwritten without warning.
	procedure mount_submodule (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		variant_parent	: in pac_assembly_variant_name.bounded_string; -- low_cost								  
		instance		: in pac_module_instance_name.bounded_string; -- OSC1
		variant_submod	: in pac_assembly_variant_name.bounded_string; -- fixed_frequency
		log_threshold	: in type_log_level);

	
	-- Removes the assembly variant of a submodule. This results in all devices
	-- of the submodule being mounted.
	procedure remove_submodule (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		variant_parent	: in pac_assembly_variant_name.bounded_string; -- low_cost								   
		instance		: in pac_module_instance_name.bounded_string; -- OSC1
		log_threshold	: in type_log_level);



	-- Returns true if the given module provides the given submodule instance.
	-- The module being searched in must be in the rig already.						
	function submodule_instance_exists (
		module		: in pac_generic_modules.cursor; -- the parent module that contains the submodule instance
		instance	: in et_general.pac_module_instance_name.bounded_string) -- OSC1
		return boolean;



	-- Returns true if the given submodule instance provides the
	-- given assembly variant. The submodule instance is searched for
	-- in the parent module indicated by cursor "module".
	-- The module being searched in must be in the rig already.						
	function assembly_variant_exists (
		module		: in pac_generic_modules.cursor; -- the parent module that contains the submodule instance
		instance	: in et_general.pac_module_instance_name.bounded_string; -- OSC1
		variant		: in pac_assembly_variant_name.bounded_string) -- low_cost				
		return boolean;
	
	
	-- Sets the file name of a submodule instance.
	procedure set_submodule_file (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		file			: in et_submodules.pac_submodule_path.bounded_string; -- the file name of the submodule like templates/oscillator.mod
		instance		: in pac_module_instance_name.bounded_string; -- OSC1
		log_threshold	: in type_log_level);


	-- Performs an in depth check on the schematic of the given module.
	-- Tests:
	-- 1. for device/submodule/netchanger ports that do not have a same named device/submodule/netchanger.
	-- 2. for device/submodule/netchanger ports that occur more than once.
	-- 3. CS: for net junctions sitting on top of each other
	-- 4. CS: for device/submodule/netchanger port that do not have a visual connection to the net
	-- 5. CS: for overlapping net segments
	-- 6. CS: unconnected ports of R, C, L (category depended)
	-- 6.1 CS: unconnected inputs
	-- 7. CS: devices with empty values
	-- 8. CS: interactive devices with empty purpose
	-- 9. CS: check partcode (conventions.validate_partcode)								  
	-- 10. units sitting on to of each other (same origin position)
	-- 11. CS: warning (or error ?) if any ports sit on top of each other. This would make the movable_tests obsolete.
	procedure check_integrity (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		log_threshold	: in type_log_level);



	-- Dumps submodule names, instances and device name offsets:
	procedure dump_tree (
		module_name		: in pac_module_name.bounded_string;
		log_threshold	: in type_log_level);

	
	-- Re(builds) the submodule tree of the given parent module.
	procedure build_submodules_tree (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		log_threshold	: in type_log_level);



	-- Generates the BOM files of all assembly variants from the given top module.
	-- The files are named after the module name and the variant name:
	procedure make_boms (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		log_threshold	: in type_log_level);



	-- Calculates the device index ranges of the given top module and all its submodules.
	-- Assigns the device names offset of the instantiated submodules.
	-- Assumes that all devices of the modules are mounted -> assembly variants ignored.
	procedure autoset_device_name_offsets (
		module_name		: in pac_module_name.bounded_string; -- the top module like motor_driver (without extension *.mod)
		log_threshold	: in type_log_level);

	
	
end et_schematic_ops.submodules;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
