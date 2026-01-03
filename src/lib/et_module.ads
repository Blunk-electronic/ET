------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               MODULE                                     --
--                                                                          --
--                               S p e c                                    --
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
--  ToDo: 
--  - clean up
--  - decompose in smaller packages


with et_schematic_geometry;
with et_schematic_coordinates;
with et_schematic_text;

with et_board_geometry;

with et_nets;
with et_netlists;
with et_submodules;

with et_module_board;				use et_module_board;

with et_assembly_variants;			use et_assembly_variants;
with et_assembly_variant_name;		use et_assembly_variant_name;

with et_meta;
with et_commit;
with et_text_content;				use et_text_content;
with et_drawing_frame;
with et_drawing_frame.schematic;
with et_numbering;

with et_design_rules;				use et_design_rules;
with et_design_rules_board;			use et_design_rules_board;

with et_net_classes;				use et_net_classes;
with et_devices_electrical;			use et_devices_electrical;
with et_devices_non_electrical;		use et_devices_non_electrical;

with et_logging;					use et_logging;



package et_module is


	-- For designs which have only a schematic, this flag goes false.
	type type_board_available is new boolean;

	
	
	type type_generic_module is record -- CS make private
		commit_index	: et_commit.type_commit_index_zero_based := 0;
		
		meta			: et_meta.type_meta; -- for both schematic and layout

		rules			: type_design_rules; -- design rules, erc rules ...
		
		description		: pac_text_content.bounded_string; -- a short description of the module

		-- schematic frame template and descriptions of individual schematic frames:
		frames			: et_drawing_frame.schematic.type_frames_schematic;
		
		grid			: et_schematic_geometry.pac_grid.type_grid; -- the drawing grid of the schematic

		board_available	: type_board_available := FALSE;

		-- ALL devices of the module independent of the assembly variant:
		devices			: pac_devices_electrical.map;
		device_commits	: type_devices_undo_redo_stack;
		-- CS wrap both selectors into a record
		
		net_classes		: pac_net_classes.map;		-- the net classes
		submods			: et_submodules.pac_submodules.map;	-- instances of submodules (boxes)
		netchangers		: et_submodules.pac_netchangers.map;-- netchangers

		-- general notes in schematic, not related to drawing frames !
		texts       	: et_schematic_text.pac_texts.list; 

		-- The nets of the module (incl. routing information for the board)
		-- containing:
		-- - strands
		-- - net segments
		-- - ports of devices, netchangers and submodules
		-- On adding, moving or deleting units the structure in 
		-- selector "net" must be updated:
		nets 	    	: et_nets.pac_nets.map;
		net_commits		: et_nets.type_nets_undo_redo_stack;
		-- CS wrap both selectors into a record
		
		-- The assembly variants of the module:
		-- - devices that are mounted or not
		-- - devices which can have a different value, partcode or purpose
		-- - variants of submodules
		assembly_variants : type_module_assembly_variants;

		
		-- Non-electrical stuff (board contours, silkscreen, documentation, ...):
		board			: type_board;
		board_commits	: type_board_undo_redo_stack;
		-- CS wrap both selectors into a record
		
		-- The tree of submodules is stored here. 
		-- NOTE: This container is exclusively used if the module is a top module.
		-- In submodules it is not used (should always be empty):
		submod_tree		: et_numbering.pac_modules.tree;

		-- The netlists containing nets of top module and submodule instances:
		-- Provide information on primary nets and their subordinated secondary nets per 
		-- assembly variant.
		netlists		: et_netlists.pac_netlists.map; -- variant name and netlist

		-- Devices which do not have a counterpart in the schematic:
		devices_non_electric			: pac_devices_non_electrical.map; -- fiducials, mounting holes, ...
		devices_non_electric_commits	: type_non_electrical_devices_undo_redo_stack;
		-- CS wrap both selectors into a record
		
		-- CS: images
		-- CS: latest view: sheet number, displayed objects, zoom, cursor position, ...
	end record;



	-- Returns the design rules (both for schematic and board):
	function get_design_rules (
		module : in type_generic_module)
		return type_design_rules;
	

	-- Returns true if schematic design rules exist:
	function design_rules_schematic_assigned (
		module : in type_generic_module)
		return boolean;

		
	-- Returns true if board design rules exist:
	function design_rules_board_assigned (
		module : in type_generic_module)
		return boolean;

		
		
	function get_grid_schematic (
		module : in type_generic_module)
		return et_schematic_geometry.pac_grid.type_grid;


	function get_grid_board (
		module : in type_generic_module)
		return et_board_geometry.pac_grid.type_grid;
	


	-- Returns true if the given assembly variant exists:
	function variant_exists (
		module	: in type_generic_module;
		variant	: in pac_assembly_variant_name.bounded_string)
		return boolean;


	function get_active_variant (
		module	: in type_generic_module)
		return pac_assembly_variant_name.bounded_string;


	-- Returns the number of available assembly variants.
	-- NOTE: The default variant is not counted !
	function get_variant_count (
		module	: in type_generic_module)
		return natural;

	
end et_module;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
