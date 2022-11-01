------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                             TERMINALS                                    --
--                                                                          --
--                              S p e c                                     --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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
with ada.characters.handling;	use ada.characters.handling;

with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;

with ada.containers.indefinite_ordered_maps;
with ada.containers.indefinite_doubly_linked_lists;

with et_general;
--with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;
with et_pcb_coordinates;		use et_pcb_coordinates;
with et_geometry;				use et_geometry;
with et_pcb_stack;				use et_pcb_stack;
with et_drills;					use et_drills;
with et_text;
with et_board_shapes_and_text;	use et_board_shapes_and_text;
with et_design_rules;			use et_design_rules;
with cairo;


package et_terminals is

	use pac_geometry_brd;
	use pac_geometry_2;
	use pac_contours;
	use pac_polygons;
	use pac_text_board;


	
	pad_size_min : constant type_track_width := 0.05;
	pad_size_max : constant type_track_width := 10.0;
	subtype type_pad_size is type_distance_positive range pad_size_min .. pad_size_max;

	-- Checks whether given pad size is in range of type_pad_size
	procedure validate_pad_size (size : in type_distance);

	

	pad_drill_offset_min : constant type_distance_positive := zero;
	pad_drill_offset_max : constant type_distance_positive := pad_size_max * 0.5;
	
	subtype type_pad_drill_offset is type_distance_positive 
		range pad_drill_offset_min .. pad_drill_offset_max;
	


	
-- PLATED MILLINGS	

	procedure log_plated_millings (
		millings 		: in type_contour;
		log_threshold	: in type_log_level);

	
	plated_millings_default : type_contour;
	-- CS this variable should never be changed.
	-- Find a way to make it a constant.

	

	
	keyword_restring_outer : constant string := "restring_outer";
	keyword_restring_inner : constant string := "restring_inner";



	
	-- The solder paste status is for compatibility with other CAE systems
	-- to account for virtual devices like net-ties or netchangers.
	type type_solder_paste_status is (NONE, APPLIED);
	solder_paste_status_default : constant type_solder_paste_status := APPLIED;
	function to_string (solder_paste : in type_solder_paste_status) return string;
	function to_solder_paste_status (solder_paste : in string) return type_solder_paste_status;



	

	
	-- The stop mask status is for compatibility with other CAE systems
	-- to account for virtual devices like net-ties or netchangers.
	type type_stop_mask_status is (CLOSED, OPEN);
	stop_mask_status_default : constant type_stop_mask_status := OPEN;
	function to_string (stop_mask : in type_stop_mask_status) return string;
	function to_stop_mask_status (stop_mask : in string) return type_stop_mask_status;


	
	-- A THT terminal may have a drilled or a milled hole (milled hole is also called "plated millings")
	type type_terminal_tht_hole is (DRILLED, MILLED);
	terminal_tht_hole_default : constant type_terminal_tht_hole := DRILLED;
	function to_string (tht_hole : in type_terminal_tht_hole) return string;
	function to_tht_hole (tht_hole : in string) return type_terminal_tht_hole;

	
	-- A pad outline is a polygon:
	type type_pad_outline_tht is record
		top		: type_contour; -- The shape on the top side
		bottom	: type_contour; -- is not nessecarily the same as on the bottom side.
	end record;



	
-- SOLDER STOP MASK
	
	type type_stop_mask_shape is (
		AS_PAD,			-- mask assumes same shape as conductor pad underneath
		EXPAND_PAD,		-- mask is sligtly greater thatn underlying conductor pad (definded by DRU)
		USER_SPECIFIC);	-- mask has user specific contours

	stop_mask_shape_default : constant type_stop_mask_shape := EXPAND_PAD;

	function to_string (shape : in type_stop_mask_shape) return string;
	function to_shape (shape : in string) return type_stop_mask_shape;

	
	type type_stop_mask_contours is new type_contour with null record;
	-- CS other properties of stop mask contours ?

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

	keyword_stop_mask_status		: constant string := "stop_mask_status";
	keyword_stop_mask_shape			: constant string := "stop_mask_shape";
	keyword_stop_mask_shape_top		: constant string := "stop_mask_shape_top";
	keyword_stop_mask_shape_bottom	: constant string := "stop_mask_shape_bottom";	


	
	
-- SOLDER CREAM / STENCIL
	type type_stencil_shape is (
		AS_PAD,			-- opening in stencil has the same size as the conductor pad underneath
		SHRINK_PAD,		-- opening sligtly smaller than conductor pad. defined by shrink_factor
		USER_SPECIFIC);	-- opening has a user defined outline

	--subtype type_stencil_shrink is type_polygon_scale range 0.2 .. 1.0;

	--stencil_shrink_default : constant type_stencil_shrink := 0.7; -- CS adjust to a useful value
	stencil_shrink_default : constant type_distance_positive := 0.7; -- CS adjust to a useful value
	-- CS subtype for shrink value ?
	
	stencil_shape_default : constant type_stencil_shape := AS_PAD;

	function to_string (shape : in type_stencil_shape) return string;
	function to_shape (shape : in string) return type_stencil_shape;

	type type_stencil_contours is new type_contour with null record;
	-- CS other properties stencil contours ?
	
	type type_stencil (shape : type_stencil_shape := stencil_shape_default) is record
		case shape is
			when USER_SPECIFIC 	=> contours : type_stencil_contours;
			when SHRINK_PAD		=> shrink_factor : type_distance_positive := stencil_shrink_default;
			when others			=> null;
		end case;
	end record;
	
	keyword_solder_paste_status			: constant string := "solder_paste_status";
	keyword_solder_paste_shape			: constant string := "solder_paste_shape";
	keyword_solder_paste_shrink_factor	: constant string := "solder_paste_shrink_factor";


	
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

		 -- drilled/milled, without meaning if technology is SMT
		tht_hole	: type_terminal_tht_hole)
	is tagged record

			position : type_position; -- position (x/y) and rotation
			-- For SMT pads this is the geometical center of the pad.
			-- The rotation has no meaning for THT pads with round shape.
			-- The rotation is useful for exotic pad contours. The operator 
			-- would be drawing the contour with zero rotation 
			-- first (which is easier). Then by applying an angle,
			-- the countour would be rotated to its final position.

			-- CS thermal_relief on/off
			
		case technology is
			when THT =>
				-- The shape of the pad on top and bottom side.
				pad_shape_tht			: type_pad_outline_tht; 

				stop_mask_status_tht	: type_stop_mask_status := stop_mask_status_default;
				-- CS: The stop mask status applies to both top and bottom of the pad.

				stop_mask_shape_tht		: type_stop_mask_tht;
				
				-- This is the width of the conductor surrounding the 
				-- hole in inner layers.
				-- Since the hole can be of any shape we do not speak about "restring".
				-- Nevertheless the type_restring_width is used.
				-- The shape of the conductor area around the hole is the same as 
				-- the shape of the hole. No further extra contours possible.
				width_inner_layers	: type_restring_width;
				
				case tht_hole is
					when DRILLED =>
						-- The hole is just a circular drilling:
						drill_size	: type_drill_size_tht;
						
					when MILLED =>
						-- This structure has a closed circumfence with
						-- an arbitrary outline (so called "plated millings").
						-- This is the inner contour of the milled hole.
						-- The conductor strip around has a width as defined
						-- by width_inner_layers (see above):
						millings	: type_contour;
				end case;

				
			when SMT =>
				pad_shape_smt			: type_contour;
				face					: type_face;
				stop_mask_status_smt	: type_stop_mask_status := stop_mask_status_default;
				stop_mask_shape_smt 	: type_stop_mask_smt;
				
				solder_paste_status		: type_solder_paste_status := solder_paste_status_default;
				stencil_shape			: type_stencil;

		end case;
	end record;


	-- For laying out traces we need a type that provides for a terminal information about
	-- x/y/rotation/technology and optionally the face. Face is available if technology is SMT.
	-- NOTE: This type is used in a package model. It uses fixed point numbers for
	-- the terminal positions because the package model is man-made.
	type type_terminal_position (technology	: type_assembly_technology) 
	is new pac_geometry_2.type_position with record
		case technology is
			when SMT => face : type_face;
			when THT => null;
		end case;
	end record;

	
	-- NOTE: This type is used in the board when inquiring for terminal positions.
	-- It uses floating point numbers for the terminal position because: 
	-- After rotating the package in the board the x/y coordinates are machine-made. 
	-- Fixed point coordinates would not be useful here.
	type type_terminal_position_fine (technology : type_assembly_technology) is record
		place		: type_vector;
		rotation	: type_angle := 0.0;
		
		case technology is
			when SMT => face : type_face;
			when THT => null;
		end case;
	end record;

	
	-- package pac_terminal_positions is new indefinite_doubly_linked_lists (type_terminal_position);

	
	
	-- A terminal is the physical point where electrical energy comes in or out of the device.
	-- Other CAE systems refer to "pins" or "pads". In order to use only a single word
	-- we further-on speak about "terminals".
	-- The name of a terminal may have 10 characters which seems sufficient for now.
	-- CS: character set, length check, charcter check
 	terminal_name_length_max : constant natural := 10;
	package pac_terminal_name is new generic_bounded_length (terminal_name_length_max);
	use pac_terminal_name;

	function to_string (terminal : in pac_terminal_name.bounded_string) return string;
	function to_terminal_name (terminal : in string) return pac_terminal_name.bounded_string;


	
	-- GUI relevant only:
	terminal_name_font : constant et_text.type_font := (
		family	=> et_text.to_family ("monospace"),
		slant	=> cairo.CAIRO_FONT_SLANT_NORMAL,
		weight	=> cairo.CAIRO_FONT_WEIGHT_NORMAL);

	terminal_name_size : constant pac_text_board.type_text_size := 0.5;
	

	
	
	-- Logs the properties of the given terminal.
	procedure terminal_properties (
		terminal		: in type_terminal;
		name			: in pac_terminal_name.bounded_string;
		log_threshold 	: in type_log_level);
	
	package pac_terminals is new indefinite_ordered_maps (
		key_type		=> pac_terminal_name.bounded_string, -- H7, 14
		element_type	=> type_terminal,
		"<"				=> pac_terminal_name."<");


	-- Iterates the terminate. Aborts the process when the proceed-flag goes false:
	procedure iterate (
		terminals	: in pac_terminals.map;
		process		: not null access procedure (position : pac_terminals.cursor);
		proceed		: not null access boolean);
	
	
end et_terminals;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
