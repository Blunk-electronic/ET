------------------------------------------------------------------------------
--                                                                          --
--                          SYSTEM ET SCHEMATIC                             --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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

--   The two letters "CS" indicate a "construction side" where things are not
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

with et_general;				use et_general;

with et_coordinates;
with et_libraries;
with assembly_variants;
with et_string_processing;
with et_pcb;
with et_pcb_coordinates;
with submodules;
with numbering;
with material;

package et_schematic is
	use et_general.type_net_name;
	
-- TEXT FIELD

	-- A text/note in the schematic:
	type type_text is new et_libraries.type_text_basic with record
		coordinates		: et_coordinates.type_coordinates; -- CS rename to position
		content			: et_libraries.type_text_content.bounded_string;
	end record;

	package type_texts is new doubly_linked_lists (type_text);

	
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
	subtype type_appearance_schematic is et_libraries.type_device_appearance 
		range et_libraries.SCH .. et_libraries.SCH_PCB;

	-- In a schematic we find units spread all over.
	-- A unit is a subsection of a device.
	type type_unit_base is tagged record
		rotation	: et_coordinates.type_rotation := et_coordinates.rotation_zero;
		mirror		: type_mirror := NO;
	end record;

	-- Some placeholders of a unit are available when the device appears in both schematic and layout:	
	type type_unit (appearance : type_appearance_schematic) is new type_unit_base with record
		position	: et_coordinates.type_coordinates;
		case appearance is
			when et_libraries.SCH => null; -- CS
			when et_libraries.SCH_PCB =>
				reference	: et_libraries.type_text_placeholder (meaning => et_libraries.reference); -- CS rename selector to "name"
				value		: et_libraries.type_text_placeholder (meaning => et_libraries.value);
				purpose		: et_libraries.type_text_placeholder (meaning => et_libraries.purpose); -- to be filled in schematic later by the user
		end case;
		-- NOTE: The placeholders are defined in et_libraries. Thus they have only
		-- basic coordinates (x/y). Via the unit position the sheet number can be obtained.
	end record;

	-- Units of a device are collected in a map.
	-- A unit is accessed by its name like "I/O Bank 3" or "PWR" or "A" or "B" ...	
	package type_units is new indefinite_ordered_maps (
		key_type		=> et_libraries.type_unit_name.bounded_string,
		"<" 			=> et_libraries.type_unit_name."<",
		element_type 	=> type_unit);

	package type_unit_positions is new ordered_maps (
		key_type		=> et_libraries.type_unit_name.bounded_string, -- A, B, IO_BANK_1
		"<" 			=> et_libraries.type_unit_name."<",
		element_type	=> et_coordinates.type_coordinates, -- sheet, x, y
		"="				=> et_coordinates."=");

	function unit_positions (units : in type_units.map) return type_unit_positions.map;
	--Returns a list of units and their coordinates in the schematic.	


	-- This is a device as it appears in the schematic.
	type type_device (appearance : type_appearance_schematic) is record
		model	: et_libraries.type_device_model_file.bounded_string; -- ../libraries/devices/transistor/pnp.dev
		units	: type_units.map; -- PWR, A, B, ...
		
		case appearance is
			-- If a device appears in both schematic and layout it has got:
			when et_libraries.sch_pcb => 
				value		: et_libraries.type_value.bounded_string; -- 470R
				partcode	: material.type_partcode.bounded_string; -- R_PAC_S_0805_VAL_100R
				purpose		: et_libraries.type_device_purpose.bounded_string; -- brightness_control
				variant		: et_libraries.type_component_variant_name.bounded_string; -- D, N

				-- This is layout related. In the layout the package has a position
				-- and placeholders for name, value and purpose.
				position			: et_pcb_coordinates.type_package_position; -- incl. angle and face
				text_placeholders	: et_pcb.type_text_placeholders;

				-- CS flags that signal whether partcode, purpose, bom are displayed or not.
				
			when et_libraries.sch => 
				null;
				
			when others => null; -- CS
		end case;
	end record;

	


	subtype type_net_label_text_size is et_coordinates.type_distance range 1.0 .. 5.0; -- unit is mm
	net_label_text_size_default : constant type_net_label_text_size := 1.3;

	function to_net_label_text_size (text : in string) return type_net_label_text_size;
	-- Converts a string to type_net_label_text_size.




	
	-- This is the port of a device as it appears in a net segment:
	type type_port_device is record
		device_name	: et_libraries.type_device_name;
		port_name	: et_libraries.type_port_name.bounded_string;
		-- CS unit name ?
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
	

	-- This is the port of a netchanger as it appears in a net segment:
	type type_port_netchanger is record
		index	: submodules.type_netchanger_id := submodules.type_netchanger_id'first;
		port	: submodules.type_netchanger_port_name := submodules.SLAVE; -- CS reasonable default ?
	end record;

	function "<" (left, right : in type_port_netchanger) return boolean;	
	package type_ports_netchanger is new ordered_sets (type_port_netchanger);
	
	
	
	type type_net_label_appearance is (
		SIMPLE,	-- a label that shows just the name of the net
		TAG 	-- a lable that shows the net name, the sheet name and the row/column
		);		-- where the net continues

	function to_string (appearance : in type_net_label_appearance) return string;
	function to_appearance (appearance : in string) return type_net_label_appearance;
	
	type type_net_label_direction is (INPUT, OUTPUT, BIDIR, TRISTATE, PASSIVE); -- POWER ?
	function to_string (direction : in type_net_label_direction) return string;
	function to_direction (direction : in string) return type_net_label_direction;

	type type_net_label_base is tagged record
		position	: et_coordinates.type_point;
		rotation	: et_coordinates.type_rotation_text := 0;
        size		: et_libraries.type_text_size := et_libraries.text_size_default;
        style		: et_libraries.type_text_style := et_libraries.type_text_style'first;
		width		: et_libraries.type_text_line_width := et_libraries.type_text_line_width'first;
	end record;
	
	type type_net_label (appearance : type_net_label_appearance) is new type_net_label_base with record
		case appearance is
			when TAG => 
				direction : type_net_label_direction;
				-- CS: coordinates of next tag of this net (by sheet coord. or area ?)
			when SIMPLE => null;
		end case;
	end record;

	package type_net_labels is new indefinite_doubly_linked_lists (type_net_label);
	

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

	type type_net_segment is record
		coordinates_start 	: et_coordinates.type_point;
		coordinates_end   	: et_coordinates.type_point;
		labels				: type_net_labels.list;
		junctions			: type_junctions;
		ports_devices		: type_ports_device.set;
		ports_submodules	: type_ports_submodule.set;
		ports_netchangers	: type_ports_netchanger.set;
	end record;

	package type_net_segments is new doubly_linked_lists (type_net_segment);

	-- A net segment is divided into three zones. Their width is the ratio
	-- of segment length and the zone_division_factor.
	-- 
	--    S---|---center---|---E
	--
	-- The position of the bar (|) in this drawing depends on the zone_division_factor.
	-- The center has twice the length of start/end point.
	type type_zone is (START_POINT, CENTER, END_POINT);
	zone_division_factor : constant positive := 4;
	
	function which_zone (
	-- Calculates the zone on the segment where point is nearest.
		point	: in et_coordinates.type_point;
		segment	: in type_net_segments.cursor) 
		return type_zone;

	function to_string (segment : in type_net_segments.cursor) return string;
	-- Returns a string that tells about start and end coordinates of the net segment.
	
	-- A strand is a collection of net segments which belong to each other. 
	-- Segments belong to each other because their start/end points meet.
	-- A strand has coordinates. 
	-- x/y position are the lowest values within the strand.
	type type_strand is record
	-- NOTE: ET does not provide a name for a strand.
	-- As a strand is part of a net, there is no need for individual strand names.
		position	: et_coordinates.type_coordinates; -- sheet and lowest x/y
		segments	: type_net_segments.list;
	end record;

	procedure set_strand_position (strand : in out type_strand);
	-- Calculates and sets the lowest x/y position of the given strand.	
	-- Leaves the sheet number of the strand as it is.	

	package type_strands is new doubly_linked_lists (type_strand);

	-- If a net exists in a (sub)module exclusively or whether it can be
	-- seen from the parent module. For example power nets like GND are global.
	type type_net_scope is (
		LOCAL,	-- parent module can connect to it via netchanger only
		GLOBAL	-- parent module can connect to it directly
		);

	function to_string (net_scope : in type_net_scope) return string;
	function to_net_scope (scope : in string) return type_net_scope;
	
	type type_net is new type_net_base with record
		strands		: type_strands.list;
		scope		: type_net_scope := LOCAL;
	end record;
	
	package type_nets is new ordered_maps (
		key_type		=> type_net_name.bounded_string,
		element_type	=> type_net);


	type type_ports is record
		devices		: type_ports_device.set;
		submodules	: type_ports_submodule.set;
		netchangers	: type_ports_netchanger.set;
	end record;
	
	function ports (net : in type_nets.cursor) return type_ports;
	-- Returns the ports of devices, submodules and netchangers in
	-- the given net.

-- MODULE

	-- The devices of a module are collected in a map.
 	package type_devices is new indefinite_ordered_maps (
		key_type		=> et_libraries.type_device_name, -- something like "IC43"
		"<"				=> et_libraries."<",
 		element_type	=> type_device);

	function package_model (device : in type_devices.cursor)
		return et_libraries.type_package_model_file.bounded_string; -- libraries/packages/smd/SOT23.pac
	-- Returns the name of the package model of the given device.
	
	-- For designs which have only a schematic, this flag goes false.
	type type_board_available is new boolean;


	type type_module is record
		board_available	: type_board_available := FALSE;
		
		devices			: type_devices.map;						-- the devices of the module
		net_classes		: et_pcb.type_net_classes.map;			-- the net classes
		submods			: submodules.type_submodules.map;		-- submodules
		netchangers		: submodules.type_netchangers.map;		-- netchangers
		
		frame_template_schematic	: et_libraries.type_frame_template_name.bounded_string;	-- $ET_FRAMES/drawing_frame_version_1.frm
		
		-- CS frame_count_schematic		: et_coordinates.type_submodule_sheet_number := et_coordinates.type_submodule_sheet_number'first; -- 10 frames
		-- should be part of statistics
		
		frame_template_board		: et_libraries.type_frame_template_name.bounded_string;	-- $ET_FRAMES/drawing_frame_version_2.frm
		-- CS: handle sheet description via a composite type consisting of template name and a bounded string
		
		texts       	: type_texts.list; -- general notes, not related to drawing frames !
		-- CS: images

		-- the nets of the module (incl. routing information from the board):
		nets 	    	: type_nets.map;

		-- the assembly variants of the module
		variants		: assembly_variants.type_variants.map;
		
		-- General non-component related board stuff (silk screen, documentation, ...):
		board			: et_pcb.type_board;

		-- The tree of submodules is stored here:
		submod_tree		: numbering.type_modules.tree;
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
