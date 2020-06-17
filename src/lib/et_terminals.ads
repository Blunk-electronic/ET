------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             TERMINALS                                    --
--                                                                          --
--                              S p e c                                     --
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
--   to do:


with ada.text_io;				use ada.text_io;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;

with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with et_general;
with et_string_processing;		use et_string_processing;
with et_pcb_coordinates;		use et_pcb_coordinates;
with et_geometry;				use et_geometry;
with et_pcb_stack;				use et_pcb_stack;
with et_text;

with cairo;

with et_drills;					use et_drills;

package et_terminals is
	use pac_geometry_brd;
	
	-- Instantiation of the shapes package:
	package pac_shapes is new et_geometry.generic_pac_shapes (et_pcb_coordinates.pac_geometry_brd);
	use pac_shapes;

	text_size_min 		: constant type_distance_positive := 0.5;
	text_size_max 		: constant type_distance_positive := 100.0;
	text_size_default 	: constant type_distance_positive := 1.5;
	
	line_width_min 		: constant type_distance_positive := 0.15;
	line_width_max 		: constant type_distance_positive := 10.0;
	line_width_default 	: constant type_distance_positive := 0.15;

	
	-- Instantiation of the text package:
	package pac_text is new et_text.generic_pac_text (
		pac_shapes			=> pac_shapes,
		size_min			=> text_size_min,
		size_max			=> text_size_max,
		size_default		=> text_size_default,
		line_width_min		=> line_width_min,
		line_width_max		=> line_width_max,
		line_width_default	=> line_width_default
		);

	
	-- COPPER STRUCTURES GENERAL
	copper_structure_size_min : constant et_pcb_coordinates.type_distance := 0.05;
	copper_clearance_min : constant et_pcb_coordinates.type_distance := copper_structure_size_min;
	

	-- SIGNALS
	subtype type_track_clearance is type_distance_positive range copper_clearance_min .. et_pcb_coordinates.type_distance'last;

	procedure validate_track_clearance (clearance : in et_pcb_coordinates.type_distance);
	-- Checks whether the given track clearance is in range of type_track_clearance.

	track_width_max : constant type_distance_positive := 100.0;
	subtype type_track_width is type_distance_positive range copper_structure_size_min .. track_width_max;

	procedure validate_track_width (track_width : in type_distance_positive);
	-- Checks whether the given track width is in range of type_track_width.

	
	
	pad_size_min : constant type_track_width := 0.05;
	pad_size_max : constant type_track_width := 10.0;
	subtype type_pad_size is type_distance_positive range pad_size_min .. pad_size_max;

	procedure validate_pad_size (size : in et_pcb_coordinates.type_distance);
	-- Checks whether given pad size is in range of type_pad_size


	pad_drill_offset_min : constant type_distance_positive := zero;
	pad_drill_offset_max : constant type_distance_positive := pad_size_max * 0.5;
	subtype type_pad_drill_offset is type_distance_positive range pad_drill_offset_min .. pad_drill_offset_max;
	
	
	


	


	-- RESTRING
	keyword_restring_outer_layers : constant string := "restring_outer_layers";
	keyword_restring_inner_layers : constant string := "restring_inner_layers";		

	restring_width_max : constant type_distance_positive := 5.0;
	subtype type_restring_width is type_distance_positive range copper_structure_size_min .. restring_width_max;

	procedure validate_restring_width (restring_width : in et_pcb_coordinates.type_distance);
	-- Checks whether the given restring width is in range of type_restring_width.


	

	
-- PLATED MILLINGS OF TERMINALS	
	-- Plated millings as used by terminals. These structures have closed circumfence.
	type type_plated_millings is new pac_shapes.type_polygon_base with null record;

	procedure log_plated_millings (
		millings 		: in type_plated_millings;
		log_threshold	: in et_string_processing.type_log_level);

	
	type type_solder_paste_status is (NONE, APPLIED);
	solder_paste_status_default : constant type_solder_paste_status := APPLIED;
	function to_string (solder_paste : in type_solder_paste_status) return string;
	function to_solder_paste_status (solder_paste : in string) return type_solder_paste_status;
	
	type type_stop_mask_status is (CLOSED, OPEN);  -- net-ties or netchangers have their pads covered
	stop_mask_status_default : constant type_stop_mask_status := OPEN;
	function to_string (stop_mask : in type_stop_mask_status) return string;
	function to_stop_mask_status (stop_mask : in string) return type_stop_mask_status;
	
	-- A THT terminal may have a drilled or a milled hole (milled hole is also called "plated millings")
	type type_terminal_tht_hole is (DRILLED, MILLED);
	terminal_tht_hole_default : constant type_terminal_tht_hole := DRILLED;
	function to_string (tht_hole : in type_terminal_tht_hole) return string;
	function to_tht_hole (tht_hole : in string) return type_terminal_tht_hole;

	
	-- A pad outline is a polygon:
	type type_pad_outline is new pac_shapes.type_polygon_base with null record;
	
	type type_pad_outline_tht is record
		top		: type_pad_outline; -- The shape on the top side
		bottom	: type_pad_outline; -- is not nessecarily the same as on the bottom side.
	end record;


-- SOLDER STOP MASK
	
	type type_stop_mask_shape is (
		AS_PAD,			-- mask assumes same shape as underlying conductor pad
		EXPAND_PAD,		-- mask is sligtly greater thatn underlying conductor pad (definded by DRU)
		USER_SPECIFIC);	-- mask has user specific contours

	stop_mask_shape_default : constant type_stop_mask_shape := EXPAND_PAD;

	function to_string (shape : in type_stop_mask_shape) return string;
	function to_shape (shape : in string) return type_stop_mask_shape;
	
	type type_stop_mask_contours is new pac_shapes.type_polygon_base with null record;

	-- Contours of stop mask are required only if the shape is user specific.
	-- Otherwise the shape is to be derived from the underlying conductor pad and
	-- the DRU settings:
	type type_stop_mask (shape : type_stop_mask_shape := stop_mask_shape_default) is record
		case shape is
			when USER_SPECIFIC => contours : type_stop_mask_contours;
			when others => null;
		end case;
	end record;

	-- A THT pad has stop mask on top and bottom side:
	type type_stop_mask_tht is record
		top		: type_stop_mask; -- The shape on the top side
		bottom	: type_stop_mask; -- is not nessecarily the same as on the bottom side.
	end record;

	-- A SMT pad has stop mask on one side only:
	subtype type_stop_mask_smt is type_stop_mask;
	
	
	
	keyword_stop_mask			: constant string := "stop_mask";
	keyword_solder_paste		: constant string := "solder_paste";

	keyword_stop_mask_shape			: constant string := "stop_mask_shape";
	keyword_stop_mask_shape_top		: constant string := "stop_mask_shape_top";
	keyword_stop_mask_shape_bottom	: constant string := "stop_mask_shape_bottom";	
	
	keyword_pad_shape			: constant string := "pad_shape";	
	keyword_width_inner_layers	: constant string := "width_inner_layers";
	keyword_assembly_technology	: constant string := "technology";
	keyword_tht_hole			: constant string := "hole";	
	keyword_drill_size			: constant string := "drill_size";

	
	type type_assembly_technology is (
		THT,	-- Through Hole Technology
		SMT		-- Surface Mount Technology
		);

	assembly_technology_default : constant type_assembly_technology := SMT;
	function to_string (technology : in type_assembly_technology) return string;
	function to_assembly_technology (technology : in string) return type_assembly_technology;
	

	
	type type_terminal (
		technology	: type_assembly_technology; -- smt/tht
		tht_hole	: type_terminal_tht_hole) -- drilled/milled, without meaning if technology is SMT
		is tagged record

			position : type_position; -- position (x/y) and rotation
			-- For SMT pads this is the geometic center of the pad.
			-- The rotation has no meaning for THT pads with round shape.
			-- The rotation is useful for exotic pad contours. The operator would be drawing the 
			-- contour with zero rotation first (which is easier). Then by applying an angle,
			-- the countour would be rotated to its final position.
			
		case technology is
			when THT =>
				-- The shape of the pad on top and bottom side.
				pad_shape_tht		: type_pad_outline_tht; 
				stop_mask_shape_tht	: type_stop_mask_tht;

				-- This is the width of the copper surrounding the hole in inner layers.
				-- Since the hole can be of any shape we do not speak about restring.
				-- The shape of the copper area around the hole is the same as the shape of the 
				-- hole. No further extra contours possible.
				width_inner_layers	: type_track_width;
				
				case tht_hole is
					when DRILLED =>
						drill_size	: type_drill_size_tht;
						
					when MILLED =>
						millings	: type_plated_millings;
				end case;
				
			when SMT =>
				pad_shape_smt		: type_pad_outline;
				face				: type_face;
				stop_mask_status	: type_stop_mask_status := stop_mask_status_default;
				stop_mask_shape_smt : type_stop_mask_smt;
				
				solder_paste_status	: type_solder_paste_status := solder_paste_status_default;
				-- CS ?? stencil_shape : type_stencil_outline;
				-- If no elements in outline, apply pad_shape.
				
		end case;
	end record;


	-- A terminal is the physical point where electrical energy comes in or out of the device.
	-- Other CAE systems refer to "pins" or "pads". In order to use only a single word
	-- we further-on speak about "terminals".
	-- The name of a terminal may have 10 characters which seems sufficient for now.
	-- CS: character set, length check, charcter check
 	terminal_name_length_max : constant natural := 10;
	package type_terminal_name is new generic_bounded_length (terminal_name_length_max);
	use type_terminal_name;

	function to_string (terminal : in type_terminal_name.bounded_string) return string;
	function to_terminal_name (terminal : in string) return type_terminal_name.bounded_string;


	
	-- GUI relevant only:
	terminal_name_font : constant et_text.type_font := (
		family	=> et_text.to_family ("monospace"),
		slant	=> cairo.CAIRO_FONT_SLANT_NORMAL,
		weight	=> cairo.CAIRO_FONT_WEIGHT_NORMAL);

	terminal_name_size : constant pac_text.type_text_size := 0.5;
	

	
	
	procedure terminal_properties (
	-- Logs the properties of the given terminal.
		terminal		: in type_terminal;
		name			: in type_terminal_name.bounded_string;
		log_threshold 	: in et_string_processing.type_log_level);
	
	package type_terminals is new indefinite_ordered_maps (
		key_type		=> type_terminal_name.bounded_string, -- H7, 14
		element_type	=> type_terminal,
		"<"				=> type_terminal_name."<");

	
end et_terminals;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
