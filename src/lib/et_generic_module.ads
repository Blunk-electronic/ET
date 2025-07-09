------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           GENERIC MODULE                                 --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
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

with et_schematic_coordinates;
with et_schematic_text;
with et_nets;
with et_netlists;
with et_submodules;
with et_pcb;
with et_assembly_variants;			use et_assembly_variants;
with et_assembly_variant_name;		use et_assembly_variant_name;
with et_module_names;				use et_module_names;
with et_meta;
with et_commit;
with et_text;
with et_frames;
with et_numbering;
with et_design_rules;
with et_net_class;					use et_net_class;
with et_devices_electrical;			use et_devices_electrical;
with et_devices_non_electrical;		use et_devices_non_electrical;
with et_logging;					use et_logging;


package et_generic_module is


	-- For designs which have only a schematic, this flag goes false.
	type type_board_available is new boolean;

	
	
	type type_generic_module is record
		commit_index	: et_commit.type_commit_index_zero_based := 0;
		
		meta			: et_meta.type_meta; -- for both schematic and layout

		rules			: et_design_rules.type_design_rules; -- design rules, erc rules ...
		
		description		: et_text.pac_text_content.bounded_string; -- a short description of the module

		-- schematic frame template and descriptions of individual schematic frames:
		frames			: et_frames.type_frames_schematic;
		
		grid			: et_schematic_coordinates.pac_grid.type_grid; -- the drawing grid of the schematic

		board_available	: type_board_available := FALSE;

		-- ALL devices of the module independent of the assembly variant:
		devices			: pac_devices_sch.map;
		device_commits	: type_devices_undo_redo_stack;
		
		net_classes		: pac_net_classes.map;		-- the net classes
		submods			: et_submodules.pac_submodules.map;	-- instances of submodules (boxes)
		netchangers		: et_submodules.pac_netchangers.map;-- netchangers
		
		texts       	: et_schematic_text.pac_texts.list; -- general notes in schematic, not related to drawing frames !

		-- The nets of the module (incl. routing information for the board)
		-- containing:
		-- - strands
		-- - net segments
		-- - ports of devices, netchangers and submodules
		-- On adding, moving or deleting units the structure in 
		-- selector "net" must be updated:
		nets 	    	: et_nets.pac_nets.map;
		net_commits		: et_nets.type_nets_undo_redo_stack;
		
		-- The assembly variants of the module.
		-- (means which device is mounted or not or which device can have a different
		-- value, partcode or purpose):
		variants		: pac_assembly_variants.map;

		-- The active assembly variant:
		active_variant	: pac_assembly_variant_name.bounded_string; -- "premium"
		-- If active_variant is an empty string, then the default variant is active.

		
		-- Non-electrical stuff (board contours, silkscreen, documentation, ...):
		board			: et_pcb.type_board;
		board_commits	: et_pcb.type_board_undo_redo_stack;
		
		-- The tree of submodules is stored here. 
		-- NOTE: This container is exclusively used if the module is a top module.
		-- In submodules it is not used (should always be empty):
		submod_tree		: et_numbering.pac_modules.tree;

		-- The netlists containing nets of top module and submodule instances:
		-- Provide information on primary nets and their subordinated secondary nets per 
		-- assembly variant.
		netlists		: et_netlists.pac_netlists.map; -- variant name and netlist

		-- Devices which do not have a counterpart in the schematic:
		devices_non_electric			: pac_devices_non_electric.map; -- fiducials, mounting holes, ...
		devices_non_electric_commits	: type_non_electrical_devices_undo_redo_stack;
		
		-- CS: images
		-- CS: latest view: sheet number, displayed objects, zoom, cursor position, ...
	end record;



	
		
	-- Generic modules and submodules (which contain schematic and layout stuff)
	-- are collected here.
	-- Module names are things like "motor_driver" or "temperature_controller".
	-- Submodule names are things like "templates/clock_generator" or
	-- "$TEMPLATES/clock_generator" or "/home/user/templates/clock_generator":
	package pac_generic_modules is new ada.containers.ordered_maps (
		key_type		=> pac_module_name.bounded_string, -- motor_driver (without extension *.mod)
		"<"				=> pac_module_name."<",
		element_type	=> type_generic_module);


	use pac_generic_modules;


	-- Returns the name of the module indicated by module_cursor:
	function to_string (
		module_cursor	: in pac_generic_modules.cursor;
		quote			: in boolean := true)				   
		return string;

	

	function get_count (
		modules : in pac_generic_modules.map)
		return natural;

	
	
	generic_modules : pac_generic_modules.map;


	-- The current active module is stored here. Whenever objects of the schematic
	-- or board are to be drawn, this variable must be read.
	active_module : pac_generic_modules.cursor; -- the currently active module


	-- Returns the name of the currently active module:
	function get_active_module return string;




	
	-- Returns true if the module with the given name exists in container modules.
	function generic_module_exists (
		module : in pac_module_name.bounded_string) 
		return boolean;


	procedure validate_module_name (
		module : in pac_module_name.bounded_string);


	
	
	-- Locates the given module in the global container "modules".
	function locate_module (name : in pac_module_name.bounded_string) -- motor_driver (without extension *.mod)
		return pac_generic_modules.cursor;
	-- CS rename to get_module_cursor


	-- Fetches the meta information for the whole 
	-- module (both schematic and board):
	function get_meta_information (
		module : in pac_generic_modules.cursor)
		return et_meta.type_meta;


	
	-- Returns true if the given module provides the given assembly variant.
	-- If the variant is an empty string then it is about the default variant
	-- which is always provided. The return is true in that case.
	function assembly_variant_exists (
		module		: in pac_generic_modules.cursor;
		variant		: in pac_assembly_variant_name.bounded_string) -- low_cost
		return boolean;	


	
end et_generic_module;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
