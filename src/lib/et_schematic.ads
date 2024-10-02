------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              SCHEMATIC                                   --
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
--		1. Objects like net segments, net labels, notes ... 
--		   should be collected in ordered sets instead of doubly_linked_lists
--			- the benefits: placing identical objects at the same position would be impossible
--			- the cons: ordering subprograms required
--		3. device accessories

with ada.text_io;				use ada.text_io;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded;       use ada.strings.bounded;
with ada.containers;            use ada.containers;
with ada.containers.vectors;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with cairo;						--use cairo;


with et_general;				use et_general;
with et_nets;					use et_nets;
with et_net_names;				use et_net_names;
with et_sheets;					use et_sheets;
with et_coordinates_2;			use et_coordinates_2;
with et_assembly_variants;		use et_assembly_variants;
with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;

with et_schematic_shapes_and_text;		use et_schematic_shapes_and_text;
with et_device_placeholders;	--use et_device_placeholders;
with et_device_placeholders.packages;
with et_device_placeholders.symbols;	use et_device_placeholders.symbols;

with et_packages;				use et_packages;
with et_pcb;
with et_pcb_coordinates_2;
with et_submodules;
with et_numbering;
with et_material;
with et_netlists;
with et_geometry;
with et_text;
with et_symbols;				use et_symbols;
with et_devices;				use et_devices;
with et_frames;
with et_meta;
with et_design_rules;
with et_commit;
with et_object_status;			use et_object_status;


package et_schematic is

	use pac_net_name;
	use pac_unit_name;
	
	use pac_geometry_2;

	
-- TEXT FIELD

	-- GUI relevant only: The font of a text/note in the schematic:
	text_font : constant et_text.type_font := (
		family	=> et_text.to_family ("monospace"),
		slant	=> cairo.CAIRO_FONT_SLANT_NORMAL,
		weight	=> cairo.CAIRO_FONT_WEIGHT_NORMAL);

	
	-- A text/note in the schematic:
	type type_text is new pac_text_schematic.type_text with record
		position	: type_vector_model;
		rotation	: et_text.type_rotation_documentation := et_text.HORIZONTAL;
		sheet		: type_sheet := type_sheet'first;
		content		: et_text.pac_text_content.bounded_string;
		--font		: et_text.type_font;
	end record;
		
	package pac_texts is new doubly_linked_lists (type_text);


	
	
	-- Units can be placed mirrored along the x or y axis or not at all.
	type type_mirror is (NO, X_AXIS, Y_AXIS);

	
	function to_string (
		mirror	: in type_mirror;
		verbose	: in boolean)
		return string;
	-- returns the given mirror style as string

	function to_mirror_style (style : in string) return type_mirror;

	
	
	-- In a schematic we handle only virtual devices (like GND symbols)
	-- and those which appear in both schematic an layout (so called real devices):
	subtype type_appearance_schematic is et_symbols.type_appearance 
		range et_symbols.VIRTUAL .. et_symbols.PCB;

	
	-- In a schematic we find units spread all over.
	-- A unit is a subset of a device.
	-- Placeholders are available if the device appears in both schematic and layout:
	type type_unit (appearance : type_appearance_schematic) is record
		position	: et_coordinates_2.type_position; -- incl. rotation and sheet number
		mirror		: type_mirror := NO;
		case appearance is
			when et_symbols.VIRTUAL => null; -- CS
			when et_symbols.PCB =>
				name	: type_text_placeholder (meaning => et_device_placeholders.NAME);
				value	: type_text_placeholder (meaning => et_device_placeholders.VALUE);
				purpose	: type_text_placeholder (meaning => et_device_placeholders.PURPOSE); -- to be filled in schematic later by the user
		end case;
		-- NOTE: The placeholders are defined in et_symbols. Thus they have only
		-- basic coordinates (x/y relative to the unit position).
		-- Via the unit position the sheet number can be obtained.
	end record;

	
	-- Units of a device are collected in a map.
	-- A unit is accessed by its name like "I/O Bank 3" or "PWR" or "A" or "B" ...	
	package pac_units is new indefinite_ordered_maps (
		key_type		=> pac_unit_name.bounded_string,
		element_type 	=> type_unit);

	
	-- Returns a string that tells the name and position of given unit.
	function to_string (unit : in pac_units.cursor) return string;



	
	package pac_unit_positions is new ordered_maps (
		key_type		=> pac_unit_name.bounded_string, -- A, B, IO_BANK_1
		element_type	=> et_coordinates_2.type_position); -- sheet, x, y

	
	function unit_positions (units : in pac_units.map) return pac_unit_positions.map;
	--Returns a list of units and their coordinates in the schematic.	


	-- This is a device as it appears in the schematic.
	type type_device_sch (appearance : type_appearance_schematic) is record

		-- The link to the device model like ../libraries/devices/transistor/pnp.dev
		model	: pac_device_model_file.bounded_string;

		-- The units like PWR, A, B, ...
		-- Virtual devices have only one unit (like the GND symbol).
		-- Real devices like a single resistor have one unit.
		-- Real devices like FPGAs have many units (like PWR1, PWR2, GPIO1, GPIO2, ...):
		units	: pac_units.map;
		
		case appearance is
			-- If a device appears in both schematic and layout it has got:
			when et_symbols.PCB =>
				value		: pac_device_value.bounded_string; -- 470R
				
				partcode	: pac_device_partcode.bounded_string; -- R_PAC_S_0805_VAL_100R
				-- For virtual packages (test points, edge connectors, ...)
				-- usually no partcode is required.

				-- The purpose indicates what the device is doing.
				-- It is usually required for devices that require interaction
				-- with the user of a PCBA:
				purpose		: pac_device_purpose.bounded_string; -- brightness_control

				-- The package variant:
				variant		: pac_package_variant_name.bounded_string; -- D, N

				-- This is layout related. In the layout the package has a position
				-- and placeholders for name, value and purpose.
				-- The assembly side of a packages is by default TOP.
				-- The flag "flipped" indicates whether the package has been flipped
				-- in the layout drawing by the operator.
				-- As a result of a flip operation, position.face changes from top to bottom
				-- or vice versa.
				-- Flipping a device to top or bottom means to mirror it along its Y-axis.
				position			: et_pcb_coordinates_2.type_package_position; -- incl. rotation and face
				flipped				: type_flipped := flipped_default;
				text_placeholders	: et_device_placeholders.packages.type_text_placeholders;

				-- CS flags that signal whether partcode, purpose, bom are displayed or not.

				status : type_object_status;
				
			when et_symbols.VIRTUAL => null;

		end case;
	end record;



	
	
	-- Returns a cursor to the strand that is
	-- on the given sheet and has the lowest x/y position.
	-- Returns no_element if the given sheet does not
	-- contain a strand of the given net.
	function get_first_strand_on_sheet (
		sheet		: in type_sheet;
		net_cursor	: in pac_nets.cursor)
		return pac_strands.cursor;

	
	-- Returns a cursor to the strand that is
	-- on the lowest sheet and lowest x/y position:
	function get_first_strand (
		net_cursor	: in pac_nets.cursor)
		return pac_strands.cursor;
	

	
	-- A stub of a net is modelled this way:
	type type_stub_direction is (
		LEFT,	-- dead end points to the left
		RIGHT,	-- dead end points to the right
		UP,		-- dead end points up
		DOWN);	-- dead end points down

	
	type type_stub (is_stub : boolean) is record
		case is_stub is
			when TRUE => direction : type_stub_direction;
			when FALSE => null;
		end case;
	end record;

	
	-- Maps from stub direction to rotation:
	function to_label_rotation (direction : in type_stub_direction)
		return type_rotation_model;

	
	-- Detects whether the given segment is a stub and if so
	-- detects the direction of the stub relative to the given point.
	-- If the segment is neither horizontal or vertical then it is NOT a stub.
	-- Examples: 
	-- - If point is right of a horizontal segment then then it is a stub that points to the right.
	-- - If point is above of a vertical segment then then it is a stub that points up.
	function stub_direction (
		segment	: in pac_net_segments.cursor;
		point	: in type_vector_model)
		return type_stub;
		


	
	
	-- Returns the ports of devices, submodules and netchangers in
	-- the given net. The given assembly variant determines whether certain
	-- devices should be excluded (because they may not be present in a particular
	-- assembly variant).
	-- NOTE: If no variant is given, then the default variant is assumend
	-- and ALL devices are returned.
	function get_ports (
		net		: in pac_nets.cursor;
		variant	: in pac_assembly_variants.cursor := pac_assembly_variants.no_element)
		return type_ports;


	

	-- The devices of a module are collected in a map.
	-- CS: This must be a hashed map:
 	package pac_devices_sch is new indefinite_ordered_maps (
		key_type		=> type_device_name, -- something like "IC43"
 		element_type	=> type_device_sch);

	use pac_devices_sch;
	


	
	
	-- COMMITS OF ELECTRICAL DEVICES (required for undo/redo operations via the GUI):
	use et_commit;
	
	package pac_device_commit is new pac_commit (pac_devices_sch.map);
	use pac_device_commit;
	
	package pac_device_commits is new doubly_linked_lists (
		element_type	=> pac_device_commit.type_commit);

	type type_devices_undo_redo_stack is record
		dos		: pac_device_commits.list;
		redos	: pac_device_commits.list;
	end record;

	
	
	-- Iterates the devices. Aborts the process when the proceed-flag goes false:
	procedure iterate (
		devices	: in pac_devices_sch.map;
		process	: not null access procedure (position : in pac_devices_sch.cursor);
		proceed	: not null access boolean);

	
		
	

	
	
	-- For designs which have only a schematic, this flag goes false.
	type type_board_available is new boolean;

	-- As there are assembly variants, for each of them a dedicated netlist must be generated.
	package pac_netlists is new ordered_maps (
		key_type		=> pac_assembly_variant_name.bounded_string, -- low_cost, empty if default variant
		"<"				=> pac_assembly_variant_name."<",
		element_type	=> et_netlists.pac_netlist.tree, -- provides info on primary and secondary net dependencies
		"="				=> et_netlists.pac_netlist."=");





	-- To distinguish between electrical and non-electrical devices
	-- use this type:
	type type_device_category is (ELECTRICAL, NON_ELECTRICAL);
	

	
	procedure device_name_in_use (
		name	: in type_device_name;	-- IC1, MH1, ...
		by_cat	: in type_device_category);	-- electrical/non-electrical

	
	-- For the design rules we simply refer to the file where the rules are
	-- written like JLP_ML4_standard.dru.
	-- The content of the file itself will later be stored in
	-- project wide collection of design rules et_design_rules.design_rules.
	type type_rules is record
		layout		: et_design_rules.pac_file_name.bounded_string; -- JLP_ML4_standard.dru
		-- CS ERC rules ?
	end record;
	


	

	
-- MODULE
												 
	type type_module is record
		commit_index	: et_commit.type_commit_index_zero_based := 0;
		
		meta			: et_meta.type_meta; -- for both schematic and layout

		rules			: type_rules; -- design rules, erc rules ...
		
		description		: et_text.pac_text_content.bounded_string; -- a short description of the module

		-- schematic frame template and descriptions of individual schematic frames:
		frames			: et_frames.type_frames_schematic;
		
		grid			: pac_grid.type_grid; -- the drawing grid of the schematic

		board_available	: type_board_available := FALSE;

		-- ALL devices of the module independent of the assembly variant:
		devices			: pac_devices_sch.map;
		device_commits	: type_devices_undo_redo_stack;
		
		net_classes		: et_pcb.pac_net_classes.map;		-- the net classes
		submods			: et_submodules.pac_submodules.map;	-- instances of submodules (boxes)
		netchangers		: et_submodules.pac_netchangers.map;-- netchangers
		
		texts       	: pac_texts.list; -- general notes in schematic, not related to drawing frames !

		-- The nets of the module (incl. routing information for the board)
		-- containing:
		-- - strands
		-- - net segments
		-- - ports of devices, netchangers and submodules
		-- On adding, moving or deleting units the structure in 
		-- selector "net" must be updated:
		nets 	    	: pac_nets.map;
		net_commits		: type_nets_undo_redo_stack;
		
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
		netlists		: pac_netlists.map; -- variant name and netlist

		-- Devices which do not have a counterpart in the schematic:
		devices_non_electric : et_pcb.pac_devices_non_electric.map; -- fiducials, mounting holes, ...
		devices_non_electric_commits : et_pcb.type_non_electrical_devices_undo_redo_stack;
		
		-- CS: images
		-- CS: latest view: sheet number, displayed objects, zoom, cursor position, ...
	end record;




	
-- MISC
	type type_danger is (
		FLOATING_INPUT,
		CONTENTION,
		SHORT_CIRCUIT,
		NO_POWER_SUPPLY,
		NOT_PREDICTABLE
		);
	
	function show_danger (danger : in type_danger) return string;
		
end et_schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
