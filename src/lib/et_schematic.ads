------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              SCHEMATIC                                   --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
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

with et_coordinates;
with assembly_variants;
with et_string_processing;
with et_packages;
with et_pcb;
with et_pcb_coordinates;
with submodules;
with numbering;
with material;
with netlists;
with et_geometry;
with et_text;
with et_symbols;
with et_devices;
with et_frames;
with et_meta;

package et_schematic is
	use et_general.type_net_name;
	use et_coordinates.pac_geometry_sch;

	package pac_shapes is new et_geometry.generic_pac_shapes (et_coordinates.pac_geometry_sch);
	use pac_shapes;

	package pac_text is new et_text.generic_pac_text (
		pac_shapes			=> pac_shapes,
		size_min			=> et_symbols.text_size_min,
		size_max			=> et_symbols.text_size_max,
		size_default		=> et_symbols.text_size_default,
		line_width_min		=> et_symbols.text_line_width_min,
		line_width_max		=> et_symbols.text_line_width_max,
		line_width_default	=> et_symbols.text_line_width_default
		);

	use pac_text;
	
-- TEXT FIELD

	-- GUI relevant only: The font of a text/note in the schematic:
	text_font : constant et_text.type_font := (
		family	=> et_text.to_family ("monospace"),
		slant	=> cairo.CAIRO_FONT_SLANT_NORMAL,
		weight	=> cairo.CAIRO_FONT_WEIGHT_NORMAL);
	
	-- A text/note in the schematic:
	type type_text is new pac_text.type_text with record
		position	: et_coordinates.pac_geometry_sch.type_point;
		rotation	: et_text.type_rotation_documentation;
		sheet		: et_coordinates.type_sheet;
		content		: et_text.type_text_content.bounded_string;
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
	-- A unit is a subsection of a device.
	-- Some placeholders of a unit are available when the device appears in both schematic and layout:
	type type_unit (appearance : type_appearance_schematic) is record
		position	: et_coordinates.type_position; -- incl. rotation and sheet number
		mirror		: type_mirror := NO;
		case appearance is
			when et_symbols.VIRTUAL => null; -- CS
			when et_symbols.PCB =>
				name	: et_symbols.type_text_placeholder (meaning => et_symbols.NAME);
				value	: et_symbols.type_text_placeholder (meaning => et_symbols.VALUE);
				purpose	: et_symbols.type_text_placeholder (meaning => et_symbols.PURPOSE); -- to be filled in schematic later by the user
		end case;
		-- NOTE: The placeholders are defined in et_symbols. Thus they have only
		-- basic coordinates (x/y). Via the unit position the sheet number can be obtained.
	end record;

	-- Units of a device are collected in a map.
	-- A unit is accessed by its name like "I/O Bank 3" or "PWR" or "A" or "B" ...	
	package type_units is new indefinite_ordered_maps (
		key_type		=> et_devices.type_unit_name.bounded_string,
		"<" 			=> et_devices.type_unit_name."<",
		element_type 	=> type_unit);

	package type_unit_positions is new ordered_maps (
		key_type		=> et_devices.type_unit_name.bounded_string, -- A, B, IO_BANK_1
		"<" 			=> et_devices.type_unit_name."<",
		element_type	=> et_coordinates.type_position, -- sheet, x, y
		"="				=> et_coordinates."=");

	function unit_positions (units : in type_units.map) return type_unit_positions.map;
	--Returns a list of units and their coordinates in the schematic.	


	-- This is a device as it appears in the schematic.
	type type_device (appearance : type_appearance_schematic) is record
		model	: et_devices.type_device_model_file.bounded_string; -- ../libraries/devices/transistor/pnp.dev
		units	: type_units.map; -- PWR, A, B, ...
		
		case appearance is
			-- If a device appears in both schematic and layout it has got:
			when et_symbols.PCB =>
				value		: et_devices.type_value.bounded_string; -- 470R
				partcode	: material.type_partcode.bounded_string; -- R_PAC_S_0805_VAL_100R
				purpose		: et_devices.type_purpose.bounded_string; -- brightness_control
				variant		: et_devices.type_variant_name.bounded_string; -- D, N

				-- This is layout related. In the layout the package has a position
				-- and placeholders for name, value and purpose.
				-- The assembly side of a packages is by default TOP.
				-- The flag "flipped" indicates whether the package has been flipped
				-- in the layout drawing by the operator.
				-- As a result of a flip operation, position.face changes from top to bottom
				-- or vice versa.
				-- Flipping a device to top or bottom means to mirror it along its Y-axis.
				position			: et_pcb_coordinates.type_package_position; -- incl. rotation and face
				flipped				: et_pcb.type_flipped := et_pcb.flipped_default;
				text_placeholders	: et_packages.type_text_placeholders;

				-- CS flags that signal whether partcode, purpose, bom are displayed or not.
				
			when et_symbols.VIRTUAL => null;

		end case;
	end record;



	subtype type_net_label_text_size is et_coordinates.type_distance range 1.0 .. 5.0; -- unit is mm
	net_label_text_size_default : constant type_net_label_text_size := 1.3;
	
	function to_net_label_text_size (text : in string) return type_net_label_text_size;
	-- Converts a string to type_net_label_text_size.



	
	-- This is the port of a device as it appears in a net segment:
	type type_port_device is record
		device_name	: et_devices.type_name;
		unit_name	: et_devices.type_unit_name.bounded_string;
		port_name	: et_symbols.type_port_name.bounded_string;
	end record;

	function "<" (left, right : in type_port_device) return boolean;
	package type_ports_device is new ordered_sets (type_port_device);

	
	-- This is the port of a submodule:
	type type_port_submodule is record
		-- The instance of a certain submodule:
		module_name	: et_general.type_module_instance_name.bounded_string; -- MOT_DRV_3

		-- The net of the submodule is here the port name:
		port_name	: type_net_name.bounded_string; -- CLOCK_GENERATOR_OUT
	end record;

	function "<" (left, right : in type_port_submodule) return boolean;
	package type_ports_submodule is new ordered_sets (type_port_submodule);
	

	
	
	type type_net_label_appearance is (
		SIMPLE,	-- a label that shows just the name of the net
		TAG 	-- a lable that shows the net name, the sheet name and the row/column
		);		-- where the net continues

	function to_string (appearance : in type_net_label_appearance) return string;
	function to_appearance (appearance : in string) return type_net_label_appearance;
	
	type type_net_label_direction is (INPUT, OUTPUT, BIDIR, TRISTATE, PASSIVE); -- POWER ?
	function to_string (direction : in type_net_label_direction) return string;
	function to_direction (direction : in string) return type_net_label_direction;

	keyword_direction : constant string := "direction";
	
	type type_net_label_base is tagged record
		position	: et_coordinates.pac_geometry_sch.type_point;
        size		: et_symbols.pac_text.type_text_size := et_symbols.text_size_default;
		width		: et_symbols.type_text_line_width := et_symbols.type_text_line_width'first;
	end record;
	
	type type_net_label (appearance : type_net_label_appearance) is new type_net_label_base with record
		case appearance is
			when TAG => 
				direction		: type_net_label_direction;

				-- The tag label can be rotated arbitrary about the position:
				rotation_tag	: et_coordinates.type_rotation := et_coordinates.pac_geometry_sch.zero_rotation;

			when SIMPLE =>
				-- The simple label can be read from the front or from the right:
				rotation_simple	: et_text.type_rotation_documentation := et_text.type_rotation_documentation'first;
		end case;
	end record;

	package type_net_labels is new indefinite_doubly_linked_lists (type_net_label);


	
	-- GUI relevant only: The font of a net label:
	net_label_font : constant et_text.type_font := (
		family	=> et_text.to_family ("monospace"),
		slant	=> cairo.CAIRO_FONT_SLANT_NORMAL,
		weight	=> cairo.CAIRO_FONT_WEIGHT_NORMAL);
	
	-- GUI relevant only: The line width of the box that enshroudes the net name of a tag label:
	tag_label_box_line_width : constant type_distance_positive := 0.2;

	-- GUI relevant only: The spacing between anchor point of tag label and net name:
	tag_label_text_offset : constant type_distance_positive := 1.0;

	-- GUI relevant only: The ratio of box height to text size of a tag label:
	tag_label_height_to_size_ratio : constant type_distance_positive := 1.8;

	
	-- This is a net:
	type type_net_base is tagged record
		route	: et_pcb.type_route; -- routing information -> pcb related

		-- The net class of the net: default, High_Voltage, EM/SI-critical, ...
		class 	: et_pcb.type_net_class_name.bounded_string := et_pcb.net_class_name_default;
	end record;

	-- A net junction is where segments and ports meet each other.	
	type type_junctions is record
		start_point	: boolean := false;
		end_point	: boolean := false;
	end record;

	-- GUI relevant only: In the schematic editor, the junction is drawn as follows:
	junction_radius : constant type_distance_positive := 0.5;
	type type_junction_symbol is new pac_shapes.type_circle with null record;
	junction_symbol : type_junction_symbol := (
						radius 	=> junction_radius,
						others	=> <>);
	
	net_line_width : constant et_symbols.type_line_width := et_symbols.port_line_width;	
	
	type type_net_segment is new pac_shapes.type_line with record
		labels				: type_net_labels.list;
		junctions			: type_junctions;
		ports_devices		: type_ports_device.set;
		ports_submodules	: type_ports_submodule.set;
		ports_netchangers	: netlists.type_ports_netchanger.set;
	end record;
	
	package type_net_segments is new doubly_linked_lists (type_net_segment);
	
	function to_string (segment : in type_net_segments.cursor) return string;
	-- Returns a string that tells about start and end coordinates of the net segment.

	-- A net segment may run in those directions:
	type type_net_segment_orientation is (
		HORIZONTAL,
		VERTICAL,
		SLOPING);

	-- Returns the orientation of a net segment.
	function segment_orientation (segment : in type_net_segments.cursor) 
		return type_net_segment_orientation;
	
	
	-- A strand is a collection of net segments which belong to each other. 
	-- Segments belong to each other because their start/end points meet.
	-- A strand has coordinates. 
	-- x/y position are the lowest values within the strand.
	type type_strand is record
	-- NOTE: ET does not provide a name for a strand.
	-- As a strand is part of a net, there is no need for individual strand names.
		position	: et_coordinates.type_position; -- sheet and lowest x/y, rotation doesn't matter -> always zero
		segments	: type_net_segments.list;
	end record;

	procedure set_strand_position (strand : in out type_strand);
	-- Calculates and sets the lowest x/y position of the given strand.	
	-- Leaves the sheet number of the strand as it is.	

	package type_strands is new doubly_linked_lists (type_strand);

	type type_net is new type_net_base with record
		strands		: type_strands.list;
		scope		: netlists.type_net_scope := netlists.LOCAL;
	end record;
	
	package type_nets is new ordered_maps (
		key_type		=> type_net_name.bounded_string,
		element_type	=> type_net);


	
	
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
		return et_coordinates.type_rotation;
	
	-- Detects whether the given segment is a stub and if so
	-- detects the direction of the stub relative to the given point.
	-- If the segment is neither horizontal or vertical then it is NOT a stub.
	-- Examples: 
	-- - If point is right of a horizontal segment then then it is a stub that points to the right.
	-- - If point is above of a vertical segment then then it is a stub that points up.
	function stub_direction (
		segment	: in type_net_segments.cursor;
		point	: in et_coordinates.pac_geometry_sch.type_point)
		return type_stub;
		


	
	
	type type_ports is record
		devices		: type_ports_device.set;
		submodules	: type_ports_submodule.set;
		netchangers	: netlists.type_ports_netchanger.set;
	end record;
	
	function ports (
		net		: in type_nets.cursor;
		variant	: in assembly_variants.pac_variants.cursor)
		return type_ports;
	-- Returns the ports of devices, submodules and netchangers in
	-- the given net. The given assembly variant determines whether
	-- a device should be excluded.
	-- NOTE: If variant points to no element, then the default variant is assumend
	-- and ALL devices are returned.


	

	-- The devices of a module are collected in a map.
	-- CS: This must be a hashed map:
 	package type_devices is new indefinite_ordered_maps (
		key_type		=> et_devices.type_name, -- something like "IC43"
		"<"				=> et_devices."<",
 		element_type	=> type_device);

	function package_model (device : in type_devices.cursor)
		return et_packages.type_package_model_file.bounded_string; -- libraries/packages/smd/SOT23.pac
	-- Returns the name of the package model of the given device.
	-- The given device must have appearance SCH_PCB. Otherwise constraint error arises here.	

	function has_real_package (device : in type_devices.cursor) return boolean;
	-- Returns true if the given device has a real package.
	-- The given device must have appearance SCH_PCB. Otherwise constraint error arises here.	
	
	-- For designs which have only a schematic, this flag goes false.
	type type_board_available is new boolean;

	-- As there are assembly variants, for each of them a dedicated netlist must be generated.
	package type_netlists is new ordered_maps (
		key_type		=> et_general.type_variant_name.bounded_string, -- low_cost, empty if default variant
		"<"				=> et_general.type_variant_name."<",
		element_type	=> netlists.type_netlist.tree, -- provides info on primary and secondary net dependencies
		"="				=> netlists.type_netlist."=");



	
	-- Devices which do not have a counterpart in the schematic 
	-- (like fiducials, mounting holes, heatsinks, ...). They can have
	-- terminals. But the terminals are not connected with any net.
	-- They have names like H1 (hole) or HS1 (heatsink) or FD (fiducial).
	-- We collect them in an indedfinite ordered map:
	type type_non_electric_device is new et_packages.type_package with record
		position			: et_pcb_coordinates.type_package_position; -- incl. rotation and face
		flipped				: et_pcb.type_flipped := et_pcb.flipped_default;
		text_placeholders	: et_packages.type_text_placeholders;
		package_model		: et_packages.type_package_model_file.bounded_string; -- ../lbr/packages/fiducial.pac

		value		: et_devices.type_value.bounded_string; -- CS useful ?
		partcode	: material.type_partcode.bounded_string; -- PN_21234 -- CS include whilst generating the BOM
		purpose		: et_devices.type_purpose.bounded_string; -- "stand off"
-- 		variant		: et_devices.type_variant_name.bounded_string; -- CS useful ?
	end record;

	-- CS: this should be a hashed map:
	package pac_non_electric_devices is new indefinite_ordered_maps (
		key_type		=> et_devices.type_name, -- H1, FD2, ...
		"<"				=> et_devices."<",
		element_type	=> type_non_electric_device);


	
	
-- MODULE
	type type_module is record
		meta			: et_meta.type_meta; -- for both schematic and layout
		
		description		: et_text.type_text_content.bounded_string; -- a short description of the module

		-- schematic frame template and descriptions of individual schematic frames:
		frames			: et_frames.type_frames_schematic;
		
		grid			: type_grid; -- the drawing grid of the schematic

		board_available	: type_board_available := FALSE;
		
		devices			: type_devices.map;						-- the devices of the module
		net_classes		: et_pcb.type_net_classes.map;			-- the net classes
		submods			: submodules.type_submodules.map;		-- instances of submodules (boxes)
		netchangers		: submodules.type_netchangers.map;		-- netchangers
		
		texts       	: pac_texts.list; -- general notes in schematic, not related to drawing frames !

		-- the nets of the module (incl. routing information from the board):
		nets 	    	: type_nets.map;

		-- the assembly variants of the module
		variants		: assembly_variants.pac_variants.map;
		active_variant	: et_general.type_variant_name.bounded_string; -- "premium"
		
		-- General non-component related board stuff (silk screen, documentation, ...):
		board			: et_pcb.type_board;

		-- The tree of submodules is stored here. 
		-- NOTE: This container is exclusively used if the module is a top module.
		-- In submodules it is not used (should always be empty):
		submod_tree		: numbering.type_modules.tree;

		-- The netlists containing nets of top module and submodule instances:
		-- Provide information on primary nets and their subordinated secondary nets per 
		-- assembly variant.
		netlists		: type_netlists.map; -- variant name and netlist

		-- Devices which do not have a counterpart in the schematic:
		non_electric_devices	: pac_non_electric_devices.map; -- fiducials, mounting holes, ...

		-- CS: images
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
	
	


-- STATISTICS
	-- Whenever we deal with statistic file this type should be used:
	statistic_file_name_length : constant positive := 100; -- CS: should suffice for now
	package type_statistic_file_name is new generic_bounded_length (statistic_file_name_length); 

	extension_statistics : constant string (1..4) := "stat";

	type type_statistics is private;

	type type_statistics_category is (
		COMPONENTS_MOUNTED,
		COMPONENTS_REAL,
		COMPONENTS_TOTAL,
		COMPONENTS_VIRTUAL,
		
		NETS_TOTAL,
		-- CS: nets_global, nets_hierarchic
		JUNCTIONS,
		PORTS_TOTAL,

		CONNECTORS,
		CAPACITORS,
		DIODES,
		INDUCTORS,
		INTEGRATED_CIRCUITS,
		JUMPERS,
		LEDS,
		--NETCHANGERS,
		RELAYS,
		RESISTORS,
		TESTPOINTS,
		TRANSISTORS
		-- CS: no_connection_flags
		);
	
	procedure statistics_set (
		cat			: in type_statistics_category;
		increment	: in boolean := true;
		number 		: in count_type := 0);

	function statistics_query (cat : in type_statistics_category) return count_type;
	-- Returns the number objects as specified by given category.
	
	function statistics_query (cat : in type_statistics_category) return string;
	-- Returns the number objects as specified by given category.
	
	
	private
	
		type type_statistics is record
			components_mounted	: count_type := 0;
			components_real		: count_type := 0;
			components_total	: count_type := 0;
			components_virtual	: count_type := 0;
			
			nets_total			: count_type := 0;
			junctions			: count_type := 0;
			ports_total			: count_type := 0;

			capacitors			: count_type := 0;
			connectors			: count_type := 0;
			diodes				: count_type := 0;
			inductors			: count_type := 0;			
			integrated_circuits	: count_type := 0;
			jumpers				: count_type := 0;
			leds				: count_type := 0;
			netchangers			: count_type := 0;
			relays				: count_type := 0;
			resistors			: count_type := 0;
			testpoints			: count_type := 0;
			transistors			: count_type := 0;
-- CS		ports_virtual		: count_type := 0;
-- CS		ports_real			: count_type := 0;
		end record;	

		statistics : type_statistics;
		

		
end et_schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
