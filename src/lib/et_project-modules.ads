------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           PROJECT MODULES                                --
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
--  ToDo: 
--  

with ada.containers;
with ada.containers.ordered_maps;

with et_schematic;
with et_nets;
with et_assembly_variants;		use et_assembly_variants;
with et_pcb;
with et_pcb_stack;
with et_devices;

with et_design_rules;			use et_design_rules;
with et_meta;


package et_project.modules is

	use pac_net_name;
		
	-- Generic modules and submodules (which contain schematic and layout stuff)
	-- are collected here.
	-- Module names are things like "motor_driver" or "temperature_controller".
	-- Submodule names are things like "templates/clock_generator" or
	-- "$TEMPLATES/clock_generator" or "/home/user/templates/clock_generator":
	package pac_generic_modules is new ada.containers.ordered_maps (
		key_type		=> pac_module_name.bounded_string, -- motor_driver (without extension *.mod)
		"<"				=> pac_module_name."<",
		element_type	=> et_schematic.type_module,
		"="				=> et_schematic."=");

	generic_modules : pac_generic_modules.map;


	-- The current active module is stored here. Whenever objects of the schematic
	-- or board are to be drawn, this variable must be read.
	active_module : et_project.modules.pac_generic_modules.cursor; -- the currently active module


	-- Returns the name of the currently active module:
	function get_active_module return string;


	
	
	-- Returns true if the module with the given name exists in container modules.
	function generic_module_exists (
		module : in pac_module_name.bounded_string) 
		return boolean;


	
	-- Locates the given module in the global container "modules".
	function locate_module (name : in pac_module_name.bounded_string) -- motor_driver (without extension *.mod)
		return pac_generic_modules.cursor;


	-- Fetches the meta information for the whole 
	-- module (both schematic and board):
	function get_meta_information (
		module : in pac_generic_modules.cursor)
		return et_meta.type_meta;
		

	
	-- Saves the given generic module in the current working directory.
	-- Saves the module with its own name if save_as_name is empty.
	-- If save_as_name contains something, then the module is saved
	-- with that name. 
	procedure save_module (
		module_cursor	: in pac_generic_modules.cursor;
		save_as_name	: in pac_module_name.bounded_string := to_module_name (""); -- motor_driver_test, templates/clock_generator_test
		log_threshold	: in type_log_level);
	
	

	

-- KEYWORDS

	keyword_assembly_variant		: constant string := "assembly_variant";
	keyword_not_mounted				: constant string := "not_mounted";
	keyword_class					: constant string := "class";
	keyword_scope					: constant string := "scope";	
	keyword_flipped					: constant string := "flipped";
	keyword_rotation_in_schematic	: constant string := "rotation_in_schematic";
	keyword_junction				: constant string := "junction";
	keyword_submodule				: constant string := "submodule";
	keyword_netchanger				: constant string := "netchanger";		
	keyword_height					: constant string := "height";	
	-- keyword_template				: constant string := "template";
	keyword_origin					: constant string := "origin";
	keyword_model					: constant string := "model";				
	keyword_variant					: constant string := "variant";
	keyword_mirrored				: constant string := "mirrored";

	keyword_sheet_number			: constant string := "number";
	keyword_sheet_category			: constant string := "category";
	keyword_sheet_description		: constant string := "text";

	
-- SECTION NAMES

	section_net_classes			: constant string := "[NET_CLASSES";
	section_net_class			: constant string := "[NET_CLASS";
	
	section_nets				: constant string := "[NETS";
	section_net					: constant string := "[NET";

	section_strands				: constant string := "[STRANDS";
	section_strand				: constant string := "[STRAND";

	section_segments			: constant string := "[SEGMENTS";
	section_segment				: constant string := "[SEGMENT";

	section_labels				: constant string := "[LABELS";
	section_label				: constant string := "[LABEL";
	
	section_submodules			: constant string := "[SUBMODULES";
	section_submodule			: constant string := "[SUBMODULE";

	section_drawing_frames		: constant string := "[DRAWING_FRAMES";
	section_sheet_descriptions	: constant string := "[SHEET_DESCRIPTIONS";
	section_sheet				: constant string := "[SHEET";
	
	section_schematic			: constant string := "[SCHEMATIC";

	section_devices				: constant string := "[DEVICES";
	section_device				: constant string := "[DEVICE";
	section_devices_non_electric: constant string := "[DEVICES_NON_ELECTRIC";

	section_assembly_variants	: constant string := "[ASSEMBLY_VARIANTS";
	section_assembly_variant	: constant string := "[VARIANT";	
	
	section_netchangers			: constant string := "[NETCHANGERS";
	section_netchanger			: constant string := "[NETCHANGER";

	section_meta				: constant string := "[META";
	section_rules				: constant string := "[RULES";
	
	section_units				: constant string := "[UNITS";

	section_port_begin			: constant string := "[PORT";

	section_preferred_libraries	: constant string := "[PREFERRED_LIBRARIES";

	section_user_settings		: constant string := "[USER_SETTINGS";

	section_outline				: constant string	:= "[OUTLINE";
	
	type type_section is ( -- CS: sort aphabetically
		SEC_DEVICES_NON_ELECTRIC,
		SEC_BOARD_LAYER_STACK,
		SEC_CONTOURS, -- of fill and cutout zones
		SEC_CUTOUT_ZONE,
		SEC_DRAWING_GRID,
		SEC_ZONE,
		SEC_HOLE,
		SEC_INIT,
		SEC_NET_CLASSES,
		SEC_NET_CLASS,
		SEC_NETS,
		SEC_NET,
		SEC_STRANDS,
		SEC_STRAND,
		SEC_SEGMENTS,
		SEC_SEGMENT,
		SEC_LABELS,
		SEC_LABEL,
		SEC_FILL_ZONES_CONDUCTOR,
		--SEC_FILL_ZONES_NON_CONDUCTOR, -- CS
		SEC_PORTS,
		SEC_PORT,
		SEC_ROUTE,
		SEC_LINE,
		SEC_ARC,
		SEC_VIA,
		SEC_VIAS,
		SEC_SUBMODULES,
		SEC_SUBMODULE,
		SEC_SHEET_DESCRIPTIONS,
		SEC_SHEET,
		SEC_DRAWING_FRAMES,
		SEC_SCHEMATIC,
		SEC_BOARD,
		SEC_DEVICES,
		SEC_DEVICE,
		SEC_ASSEMBLY_VARIANTS,
		SEC_ASSEMBLY_VARIANT,
		SEC_NETCHANGERS,
		SEC_NETCHANGER,
		SEC_META,
		SEC_TEXTS,
		SEC_TEXT,
		SEC_UNITS,
		SEC_UNIT,
		SEC_OUTLINE,
		SEC_PACKAGE,
		SEC_PLACEHOLDER,
		SEC_PLACEHOLDERS,
		SEC_PREFERRED_LIBRARIES,
		SEC_SILK_SCREEN,
		SEC_CIRCLE,
		SEC_ASSEMBLY_DOCUMENTATION,
		SEC_KEEPOUT,
		SEC_ROUTE_RESTRICT,
		SEC_RULES,
		SEC_VIA_RESTRICT,
		SEC_STOP_MASK,
		SEC_STENCIL,
		SEC_CONDUCTOR,
		SEC_PCB_CONTOURS_NON_PLATED,
		-- CS SEC_PCB_CONTOUR_PLATED
		SEC_TOP,
		SEC_BOTTOM,
		SEC_USER_SETTINGS
		);

	function to_string (section : in type_section) return string;
	-- Converts a section like SEC_NET to a string "net".

	
	-- Reads a module file and stores its content as generic module in container modules.
	-- The file name may contain environment variables.
	-- The file must exist, must be visible from the current working directory.
	procedure read_module (
		file_name 		: in string; -- motor_driver.mod, templates/clock_generator.mod
		log_threshold	: in type_log_level);

	
	-- Creates an empty generic module in container modules.
	-- Does not create the actual module file if the module
	-- name is "untitled". If the module name is something other
	-- than "untitled" then the module file will also be created.
	procedure create_module (
		module_name		: in pac_module_name.bounded_string; -- motor_driver, templates/clock_generator
		log_threshold	: in type_log_level);

	
	-- Saves a generic module (from container generic_modules) in a file inside 
	-- the current project directory.
	-- The module must be inside the current project. If it is outside
	-- the project, a warning will be issued and it will NOT be saved.
	-- If the module is outside the project directory then it will not be touched.
	-- If the module does not exist, a warning will be issued.
	procedure save_module (
		module_name		: in pac_module_name.bounded_string; -- motor_driver, templates/clock_generator
		log_threshold	: in type_log_level);

	
	-- Deletes a generic module (from container generic_modules) and
	-- the module file (*.mod) itself.
	procedure delete_module (
		module_name		: in pac_module_name.bounded_string; -- motor_driver, templates/clock_generator
		log_threshold	: in type_log_level);
	

	
	-- Returns true if the given module provides the given assembly variant.
	-- If the variant is an empty string then it is about the default variant
	-- which is always provided. The return is true in that case.
	function assembly_variant_exists (
		module		: in pac_generic_modules.cursor;
		variant		: in pac_assembly_variant_name.bounded_string) -- low_cost
		return boolean;	


	
	-- Returns true if the given module and variant provides the given device.
	-- Assumptions: 
	-- - The module being searched in must be in the rig already.
	-- - The assembly variant must exist in the module.
	-- - The device must exist in the module.
	function device_exists (
		module	: in pac_generic_modules.cursor; -- the module like motor_driver
		variant	: in pac_assembly_variant_name.bounded_string; -- low_cost				
		device	: in et_devices.type_device_name)
		return boolean;


	
	-- Returns a cursor to the alternative device in the given module
	-- and given assembly variant.
	-- Assumptions: 
	-- - The module being searched in must be in the rig already.
	-- - The assembly variant must exist in the module.
	-- - The device must exist in the module.
	-- - The device must have an entry in the given assembly variant,
	--   otherwise the return is no_element.
	function get_alternative_device (
		module	: in pac_generic_modules.cursor; -- the module like motor_driver
		variant	: in pac_assembly_variant_name.bounded_string; -- low_cost				
		device	: in et_devices.type_device_name)
		return pac_device_variants.cursor;

	

	
	-- Returns true if a design rules file for the layout has been
	-- assigned to the given module.
	function layout_rules_assigned (
		module	: in pac_generic_modules.cursor) -- the module like motor_driver
		return boolean;

	
	-- Returns the PCB design rules of the given module:
	function get_pcb_design_rules (
		module	: in pac_generic_modules.cursor) -- the module like motor_driver
		return et_design_rules.type_design_rules; -- JLP_ML4_standard.dru

	
	function get_user_settings (
		module	: in pac_generic_modules.cursor) -- the module like motor_driver
		return et_pcb.type_user_settings;

	
	-- Returns the settings of the required net class
	-- of the given module.
	-- If the given class name is "default" then the settings
	-- are returned as defined by the design rules (DRU).
	-- Assumes that the given class exists for the module.
	-- Otherwise constraint error is raised.
	function get_net_class (
		module	: in pac_generic_modules.cursor; -- the module like motor_driver
		class	: in et_pcb.pac_net_class_name.bounded_string) -- hi-voltage, si-critical
		return et_pcb.type_net_class;

	
	-- Returns the class settings of a net in a module.
	-- If given net is no_element (freetrack) then the settings of the
	-- "default" class will be returned:
	function get_net_class (
		module	: in pac_generic_modules.cursor; -- the module like motor_driver
		net		: in et_nets.pac_nets.cursor)  -- GND, RESET_N, ...
		return et_pcb.type_net_class;


	
end et_project.modules;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
