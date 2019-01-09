------------------------------------------------------------------------------
--                                                                          --
--                          SYSTEM ET SCHEMATIC                             --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2018 Mario Blunk, Blunk electronic                 --
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
--		1. Objects like net segments, net labels, junctions, notes ... 
--		   should be collected in ordered sets instead of doubly_linked_lists
--			- the benefits: placing identical objects at the same position would be impossible
--			- the cons: ordering subprograms required
--		2. Templates via copy and via reference
--		3. Assembly variants

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

with et_general;
with et_coordinates;
with et_libraries;
with et_string_processing;
with et_pcb;
with et_pcb_coordinates;

package et_schematic is

	
-- TEXT FIELD

	-- A text/note in the schematic:
	type type_text is new et_libraries.type_text_basic with record
		coordinates		: et_coordinates.type_coordinates; -- CS rename to position
		content			: et_libraries.type_text_content.bounded_string;
	end record;

	procedure write_note_properties (
		note			: in et_schematic.type_text;
		log_threshold	: in et_string_processing.type_log_level := 0);
	-- Writes the properties of the given note

	package type_texts is new doubly_linked_lists (type_text);


-- UNITS AND COMPONENTS

	-- units can be placed mirrored along the x or y axis or not at all.
	type type_mirror is (NO, X_AXIS, Y_AXIS);

	function to_string (
		mirror	: in type_mirror;
		verbose	: in boolean)
		return string;
	-- returns the given mirror style as string

	function to_mirror_style (style : in string) return type_mirror;
	
	-- In a schematic we handle only virtual components (like GND symbols)
	-- and those which appear in both schematic an layout (so called real components):
	subtype type_appearance_schematic is et_libraries.type_component_appearance 
		range et_libraries.SCH .. et_libraries.SCH_PCB;

	-- In a schematic we find units spread all over.
	-- A unit is a subsection of a device.
	-- A unit has placeholders for text like reference (like IC303), value (like 7400), ...
	type type_unit_base is tagged record
		position		: et_coordinates.type_coordinates;
		rotation		: et_coordinates.type_angle;
		mirror			: type_mirror;
		reference		: et_libraries.type_text_placeholder (meaning => et_libraries.reference);
		value			: et_libraries.type_text_placeholder (meaning => et_libraries.value);
	end record;

	-- Some placeholders of a unit are available when the component appears in both schematic and layout:	
	type type_unit (appearance : type_appearance_schematic) is new type_unit_base with record
		case appearance is
			when et_libraries.SCH => null; -- CS
			when et_libraries.SCH_PCB =>
				purpose		: et_libraries.type_text_placeholder (meaning => et_libraries.purpose); -- to be filled in schematic later by the user
		end case;
		-- NOTE: The placeholders are defined in et_libraries. Thus they have only
		-- basic coordinates (x/y). Via the unit position the sheet number can be obtained.
	end record;

	
	-- Units of a component are collected in a map.
	-- A unit is accessed by its name like "I/O Bank 3" or "PWR" or "A" or "B" ...	
	package type_units is new indefinite_ordered_maps (
		key_type		=> et_libraries.type_unit_name.bounded_string,
		"<" 			=> et_libraries.type_unit_name."<",
		element_type 	=> type_unit);

	-- BOM STATUS
	type type_bom is (YES, NO); -- if a component is to be mounted or not
	function to_string (bom : in type_bom) return string;
	function to_bom_status (bom : in string) return type_bom;

	procedure check_bom_characters (bom : in string);
	-- Checks if given string is a bom status. Case sensitive !
	
	procedure validate_bom_status (text : in string); -- CS: requires appearance and category
	-- Validates BOM status. Case sensitive !	
	

	-- This is a component as it appears in the schematic.
	type type_device (appearance : type_appearance_schematic) is record
		model			: et_libraries.type_device_library_name.bounded_string; -- ../libraries/transistor/pnp.dev
		value			: et_libraries.type_component_value.bounded_string; -- 470R
		units			: type_units.map; -- PWR, A, B, ...
		case appearance is
			-- If a component appears in both schematic and layout it has got:
			when et_libraries.sch_pcb => 
				partcode			: et_libraries.type_component_partcode.bounded_string;
				purpose				: et_libraries.type_component_purpose.bounded_string;
				bom					: type_bom; -- whether mounted or not
				variant				: et_libraries.type_component_variant_name.bounded_string; -- D, N

				-- This is layout related. In the layout the package has a position
				-- and placeholders reference, value, purpose.
				position			: et_pcb_coordinates.type_package_position; -- incl. angle and face
				text_placeholders	: et_pcb.type_text_placeholders;
				
			when et_libraries.sch => 
				null;
				
			when others => null; -- CS
		end case;
	end record;

		
	
-- LABELS AND NETS

	-- The name of a net may have 100 characters which seems sufficient for now.
	net_name_characters : character_set := to_set (ranges => (('A','Z'),('0','9'))) or to_set ("_-#");
	net_inversion_mark : constant string (1..1) := "#";
 	net_name_length_max : constant natural := 100;
	package type_net_name is new generic_bounded_length (net_name_length_max); use type_net_name;

	procedure check_net_name_length (net : in string);
	-- Tests if the given net name is longer than allowed.
	
	procedure check_net_name_characters (
		net			: in type_net_name.bounded_string;
		characters	: in character_set := net_name_characters);
	-- Tests if the given net name contains only valid characters as specified
	-- by given character set.

	function to_net_name (net_name : in string) return type_net_name.bounded_string;
	-- Converts a string to a type_net_name.
	
	function to_string (net_name : in type_net_name.bounded_string) return string;
	-- Returns the given net name as string;

	function anonymous (net_name : in type_net_name.bounded_string) return boolean;
	-- Returns true if the given net name is anonymous.
	
	subtype type_net_label_text_size is et_coordinates.type_distance range 1.0 .. 5.0; -- unit is mm
	net_label_text_size_default : constant type_net_label_text_size := 1.3;

	function to_net_label_text_size (text : in string) return type_net_label_text_size;
	-- Converts a string to type_net_label_text_size.

	-- A net junction is where segments can be connected with each other.
	type type_net_junction is record -- CS rename to type_junction
		coordinates : et_coordinates.type_2d_point; -- CS rename to position
	end record;

	-- Junctions are to be collected in a list.
	package type_junctions is new doubly_linked_lists (type_net_junction);
	
	-- This is the port of a device:
	type type_port_component is record -- CS rename to type_port_device
		reference	: et_libraries.type_component_reference;
		name		: et_libraries.type_port_name.bounded_string;
		-- CS unit name ?
	end record;

	package type_ports_component is new doubly_linked_lists (type_port_component);

	-- This is the port of a submodule. It is the interface between the parent and 
	-- submodule:
	type type_port_submodule is record
		-- The port belongs to a certain submodule:
		module		: et_coordinates.type_submodule_name.bounded_string; -- MOT_DRV_3

		-- The port addresses a certain net inside the submodule: CS ?
		--net			: type_net_name.bounded_string; CS ?

		-- The port addresses a certain port of the net inside the submodule.
		-- The actual net name is not visible to the parent module.
		-- Inside a submodule a net can have multiple ports to the outside world.
		port		: et_libraries.type_port_name.bounded_string;

		position	: et_coordinates.type_coordinates; -- the position of the port in the parent module
		direction	: et_libraries.type_port_direction; -- CS default
	end record;

	package type_ports_submodule is new doubly_linked_lists (type_port_submodule);
		
	type type_net_label_appearance is (
		SIMPLE,	-- a label that shows just the name of the net
		TAG 	-- a lable that shows the net name, the sheet name and the row/column
		);		-- where the net continues

	function to_string (appearance : in type_net_label_appearance) return string;
	function to_appearance (appearance : in string) return type_net_label_appearance;
	
	type type_net_label_direction is (INPUT, OUTPUT, BIDIR, TRISTATE, PASSIVE);
	function to_string (direction : in type_net_label_direction) return string;
	function to_direction (direction : in string) return type_net_label_direction;

	type type_net_label_base is tagged record
		coordinates	: et_coordinates.type_2d_point; -- CS rename to position
		orientation	: et_coordinates.type_angle; -- CS rename to rotation
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
	
	-- This is the definition of a net segment with start/end point and junctions
	type type_net_segment_base is tagged record
		coordinates_start 	: et_coordinates.type_coordinates;
		coordinates_end   	: et_coordinates.type_coordinates; -- CS et_coordinates.type_2d_point ?
	end record;

	function length (segment : in type_net_segment_base) return et_coordinates.type_distance;
	-- Returns the length of the given net segment.
	
	function to_string (
		segment	: in type_net_segment_base'class;
		scope	: in et_coordinates.type_scope := et_coordinates.SHEET) return string;
	-- Returns the start and end coordinates of the given net segment.



	-- A strand is a collection of net segments which belong to each other. 
	-- Segments belong to each other because their start/end points meet.
	-- A strand has coordinates. 
	-- x/y position are the lowest values within the strand.
	type type_strand_base is tagged record
		coordinates : et_coordinates.type_coordinates; -- lowest x/y -- CS rename to position
	end record;


    -- If the name of a strand can not be identified, we default to the well proved "N$" notation:
	anonymous_net_name_prefix : constant string (1..2) := "N$";

	-- This is a net:
	type type_net_base is tagged record
		route	: et_pcb.type_route; -- routing information -> pcb related

		-- The net class of the net: default, High_Voltage, EMV-critical, ...
		class 	: et_pcb.type_net_class_name.bounded_string := et_pcb.net_class_name_default;
	end record;

	
	
	type type_net_segment is new type_net_segment_base with record
		labels			: type_net_labels.list;
		junctions		: type_junctions.list;
		component_ports	: type_ports_component.list; -- CS rename to device ports
		submodule_ports	: type_ports_submodule.list; 
		-- CS no_connections	: type_no_connection_flags.list;-- the list of no-connection-flags
	end record;

	package type_net_segments is new doubly_linked_lists (type_net_segment);
	
	type type_strand is new type_strand_base with record
	-- NOTE: ET does not provide a name for a strand.
	-- As a strand is part of a net, there is no need for individual strand names.
		segments	: type_net_segments.list;
	end record;

	package type_strands is new doubly_linked_lists (type_strand);

	-- If a net exists in a (sub)module exclusively or whether it can be
	-- seen from the parent module. For example power nets like GND are global.
	type type_net_scope is (LOCAL, GLOBAL);

	function to_string (net_scope : in type_net_scope) return string;
	function to_net_scope (scope : in string) return type_net_scope;
	
	type type_net is new type_net_base with record
		strands		: type_strands.list;
		scope		: type_net_scope := LOCAL;
	end record;
	
	package type_nets is new ordered_maps (
		key_type		=> type_net_name.bounded_string,
		element_type	=> type_net);
	

	
	function to_component_reference (
	-- Converts a string like "IC303" to a composite type_component_reference.
	-- If allow_special_charater_in_prefix is given true, the first character
	-- is allowed to be a special character. Like "#PWR07" as it is used by kicad.
	-- NOTE: Leading zeroes in the id are removed.	
		text_in : in string;
		allow_special_character_in_prefix : in boolean := false) -- CS: provide CAD system specific character set instead
		return et_libraries.type_component_reference;

	function default_component_reference return et_libraries.type_component_reference;
	-- Returns a default component reference with an empty prefix and and id 0.
	-- Used to initialize a component reference.
	
	function compare_reference (left, right : in et_libraries.type_component_reference) return boolean;
	-- Returns true if left comes before right.
	-- If left equals right, the return is false.	
	
	function equal_reference (left, right : in et_libraries.type_component_reference) return boolean;
	-- Returns true if left equals right.



-- SUBMODULES
	submodule_path_length_max : constant positive := 300;
	package type_submodule_path is new generic_bounded_length (submodule_path_length_max);

	function to_submodule_path (path : in string) return type_submodule_path.bounded_string;
	
	type type_submodule_size is record
		x, y : et_coordinates.type_distance; -- size x/y of the box
	end record;

	type type_submodule_view_mode is (
		ORIGIN,		-- references and net names displayed as drawn in the generic submodule
		INSTANCE	-- references and net names displayed after renumbering and prefixing
		);

	function to_string (view : in type_submodule_view_mode) return string;
	function to_view_mode (mode : in string) return type_submodule_view_mode;
	
	type type_submodule is record
		-- name  				: et_coordinates.type_submodule_name.bounded_string;	-- MOT_DRV_3 (will be the net prefix later on)
		file				: type_submodule_path.bounded_string; -- $ET_TEMPLATES/motor_driver.mod		
        --text_size_path		: et_libraries.type_text_size; -- CS no need, should be sized automatically by the GUI
        --text_size_instance	: et_libraries.type_text_size; -- CS no need, should be sized automatically by the GUI
		position		    : et_coordinates.type_coordinates;
		size				: type_submodule_size;
		position_in_board	: et_pcb_coordinates.type_point_2d_with_angle;
		view_mode			: type_submodule_view_mode;
		reference_offset	: et_libraries.type_component_reference_id;	-- R88 turns to R2088 or R788
	end record;

-- 	package type_submodules is new ordered_maps (
-- 		key_type		=> et_coordinates.type_submodule_name.bounded_string,
-- 		"<" 			=> et_coordinates.type_submodule_name."<",
-- 		element_type	=> type_submodule);

	package type_submodules is new ordered_maps (
		--key_type		=> type_submodule_path.bounded_string, -- $ET_TEMPLATES/motor_driver.mod
		key_type		=> et_coordinates.type_submodule_name.bounded_string, -- MOT_DRV_3 (will be the net prefix later on)													
		"<" 			=> et_coordinates.type_submodule_name."<",
		element_type	=> type_submodule);

-- MODULE

	-- The devices of a module are collected in a map.
 	package type_devices is new indefinite_ordered_maps (
		key_type		=> et_libraries.type_component_reference, -- something like "IC43"
		"<"				=> compare_reference,
 		element_type	=> type_device);

	-- For designs which have only a schematic, this flag goes false.
	type type_board_available is new boolean;


	type type_module is record
		board_available	: type_board_available := FALSE;
		
		devices			: type_devices.map;						-- the devices of the module
		net_classes		: et_pcb.type_net_classes.map;			-- the net classes
		submodules  	: type_submodules.map;					-- graphical representations of submodules
		
		frame_template_schematic	: et_libraries.type_frame_template_name.bounded_string;	-- $ET_FRAMES/drawing_frame_version_1.frm
		
		-- CS frame_count_schematic		: et_coordinates.type_submodule_sheet_number := et_coordinates.type_submodule_sheet_number'first; -- 10 frames
		-- should be part of statistics
		
		frame_template_board		: et_libraries.type_frame_template_name.bounded_string;	-- $ET_FRAMES/drawing_frame_version_2.frm
		-- CS: handle sheet description via a composite type consisting of template name and a bounded string
		
		texts       	: type_texts.list; -- general notes, not related to drawing frames !
		-- CS: images

		-- the nets of the module (incl. routing information from the board):
		nets 	    	: type_nets.map;				
		
		-- General non-component related board stuff (silk screen, documentation, ...):
		board			: et_pcb.type_board;
	end record;

	
	-- This is a single native target module used as scratch.
	-- Make sure it is cleared before use ! --> via statement: et_schematic.module := (others => <>); 
	module : et_schematic.type_module; 
	

   
-- MISC
	type type_danger is (
		FLOATING_INPUT,
		CONTENTION,
		SHORT_CIRCUIT,
		NO_POWER_SUPPLY,
		NOT_PREDICTABLE
		);
	
	function show_danger (danger : in type_danger) return string;
	
	
-- NETLISTS
	-- Whenever we deal with netlist files this type should be used:
	netlist_file_name_length : constant positive := 100; -- CS: should suffice for now
	package type_netlist_file_name is new generic_bounded_length (netlist_file_name_length); 
	--use type_netlist_file_name;

	extension_netlist : constant string (1..3) := "net";

	
-- BOM
	-- Whenever we deal with BOM files this type should be used:
	bom_file_name_length : constant positive := 100; -- CS: should suffice for now
	package type_bom_file_name is new generic_bounded_length (bom_file_name_length); 

	extension_bom : constant string (1..3) := "csv";


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
		NETCHANGERS,
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
