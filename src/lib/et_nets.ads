------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                                NETS                                      --
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
-- <http://www.gnu.org/licenses/>.   
------------------------------------------------------------------------------

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.text_io;				use ada.text_io;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.strings.bounded;       use ada.strings.bounded;
with ada.containers; 			use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.ordered_sets;
with ada.containers.ordered_maps;

with et_module_instance;		use et_module_instance;
with et_coordinates_2;			use et_coordinates_2;
with et_port_names;				use et_port_names;
with et_symbol_ports;			use et_symbol_ports;
with et_symbols;
with et_device_name;			use et_device_name;
with et_unit_name;				use et_unit_name;
with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;
with et_net_names;				use et_net_names;
with et_net_labels;				use et_net_labels;
with et_netlists;
with et_pcb;
with et_commit;
with et_sheets;					use et_sheets;


package et_nets is

	use pac_geometry_2;

	

	
	-- A net junction is where segments and ports meet each other.	
	type type_junctions is record
		start_point	: boolean := false;
		end_point	: boolean := false;
	end record;

	
	-- GUI relevant only: In the schematic editor, the junction is drawn as follows:
	junction_radius : constant type_distance_positive := 0.5;
	
	type type_junction_symbol is new type_circle with null record;
	
	junction_symbol : type_junction_symbol := (
			radius 	=> junction_radius,
			others	=> <>);


	

	

	type type_net_base is tagged record
		route	: et_pcb.type_route; -- routing information -> pcb related

		-- The net class of the net: default, High_Voltage, EM/SI-critical, ...
		class 	: et_pcb.pac_net_class_name.bounded_string := et_pcb.net_class_name_default;
	end record;


	

	-- This is the port of a device as it appears in a net segment:
	type type_device_port is record
		device_name	: type_device_name; -- IC4
		unit_name	: pac_unit_name.bounded_string; -- A
		port_name	: pac_port_name.bounded_string; -- IN1
	end record;

	function "<" (left, right : in type_device_port) return boolean;
	package pac_device_ports is new ordered_sets (type_device_port);


	function to_string (port : in type_device_port) return string;


	
	-- Iterates the device ports. 
	-- Aborts the process when the proceed-flag goes false:
	procedure iterate (
		ports	: in pac_device_ports.set;
		process	: not null access procedure (position : in pac_device_ports.cursor);
		proceed	: not null access boolean);


	
	-- This is the port of a submodule:
	type type_submodule_port is record
		-- The instance of a certain submodule:
		module_name	: pac_module_instance_name.bounded_string; -- MOT_DRV_3

		-- The net of the submodule is here the port name:
		port_name	: pac_net_name.bounded_string; -- CLOCK_GENERATOR_OUT
	end record;

	function "<" (left, right : in type_submodule_port) return boolean;
	package pac_submodule_ports is new ordered_sets (type_submodule_port);


	type type_ports is record
		devices		: pac_device_ports.set;
		submodules	: pac_submodule_ports.set;
		netchangers	: et_netlists.pac_netchanger_ports.set;
	end record;
	
	
	type type_net_segment is new type_line with record
		labels		: pac_net_labels.list;
		junctions	: type_junctions;
		ports		: type_ports;
	end record;
	
	package pac_net_segments is new doubly_linked_lists (type_net_segment);


	-- Iterates the net segments. 
	-- Aborts the process when the proceed-flag goes false:
	procedure iterate (
		segments	: in pac_net_segments.list;
		process		: not null access procedure (position : in pac_net_segments.cursor);
		proceed		: not null access boolean);


	
		
	-- Returns a string that tells about start and end coordinates of the net segment.
	function to_string (segment : in pac_net_segments.cursor) return string;

	
	-- A net segment may run in those directions:
	type type_net_segment_orientation is (
		HORIZONTAL,
		VERTICAL,
		SLOPING);

	-- Returns the orientation of a net segment.
	function segment_orientation (segment : in pac_net_segments.cursor) 
		return type_net_segment_orientation;


	

	net_line_width : constant type_distance_positive := 0.2;
	-- CS rename to net_linewidth ?



	-- A strand is a collection of net segments which belong to each other. 
	-- Segments belong to each other because their start/end points meet.
	-- A strand has coordinates. 
	-- x/y position are the lowest values within the strand.
	type type_strand is record
	-- NOTE: ET does not provide a name for a strand.
	-- As a strand is part of a net, there is no need for individual strand names.
		position	: et_coordinates_2.type_position; -- sheet and lowest x/y, rotation doesn't matter -> always zero
		segments	: pac_net_segments.list;
	end record;		


	package pac_strands is new doubly_linked_lists (type_strand);


	-- Iterates the strands. 
	-- Aborts the process when the proceed-flag goes false:
	procedure iterate (
		strands	: in pac_strands.list;
		process	: not null access procedure (position : in pac_strands.cursor);
		proceed	: not null access boolean);


	
	
	-- Calculates and sets the lowest x/y position of the given strand.	
	-- Leaves the sheet number of the strand as it is.	
	procedure set_strand_position (strand : in out type_strand);

	
	-- Returns a cursor to the segment that is
	-- on the lowest x/y position of the given strand:
	function get_first_segment (
		strand_cursor	: in pac_strands.cursor)
		return pac_net_segments.cursor;


	-- Returns the sheet number of the given strand:
	function get_sheet (
		strand_cursor	: in pac_strands.cursor)
		return type_sheet;
	
	
	-- Returns true if the given point is on the given
	-- net segment:
	function on_segment (
		segment_cursor	: in pac_net_segments.cursor;
		point			: in type_vector_model)
		return boolean;

	
	-- Returns true if the given point is on the given strand:
	function on_strand (
		strand_cursor	: in pac_strands.cursor;
		place			: in et_coordinates_2.type_position)
		return boolean;
	

	type type_net is new type_net_base with record
		strands		: pac_strands.list;
		scope		: et_netlists.type_net_scope := et_netlists.LOCAL;
	end record;


	
	-- Returns the cursor to the strand at the given place.
	-- If no strand found then the return is no_element:
	function get_strand (
		net		: in type_net;
		place	: in et_coordinates_2.type_position)
		return pac_strands.cursor;
	

	-- Returns a list of strands of the given net on the 
	-- given sheet:
	function get_strands (
		net		: in type_net;
		sheet	: in type_sheet)
		return pac_strands.list;


	-- Deletes the given list of strands in the given net:
	procedure delete_strands (
		net		: in out type_net;
		strands	: in pac_strands.list);

	
	-- Appends a strand to the strands of net:
	procedure merge_strand (
		net		: in out type_net;
		strand	: in type_strand);
	

	-- Merges strands into the strands of net
	-- NOTE: The given list of strands will be cleared:
	procedure merge_strands (
		net		: in out type_net;
		strands	: in out pac_strands.list);
	
	
	-- Merges net_2 into net_1.
	-- NOTE: All strands, tracks, vias, fill-zones and cutout-areas 
	-- of net_2 are deleted.
	procedure merge_nets (
		net_1	: in out type_net;
		net_2	: in out type_net);


	use pac_net_name;

	-- Many nets are to be collected in maps:
	package pac_nets is new ordered_maps (
		key_type		=> pac_net_name.bounded_string, -- RESET_N
		element_type	=> type_net);


	-- Returns the name of the net indicated
	-- by the given net cursor:
	function to_string (
		net_cursor : in pac_nets.cursor)
		return string;
	
	
	-- Iterates the nets. Aborts the process when the proceed-flag goes false:
	procedure iterate (
		nets	: in pac_nets.map;
		process	: not null access procedure (position : in pac_nets.cursor);
		proceed	: not null access boolean);



	
-- COMMITS (required for undo/redo operations via the GUI):
	use et_commit;
	
	package pac_net_commit is new pac_commit (pac_nets.map);
	use pac_net_commit;
	
	package pac_net_commits is new doubly_linked_lists (
		element_type	=> pac_net_commit.type_commit);

	type type_nets_undo_redo_stack is record
		dos		: pac_net_commits.list;
		redos	: pac_net_commits.list;
	end record;
	
	
end et_nets;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
