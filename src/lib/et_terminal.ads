------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         PACKAGE TERMINAL                                 --
--                                                                          --
--                              S p e c                                     --
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
-- To Do:
--
--


with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;
with et_assembly_technology;	use et_assembly_technology;
with et_stencil_mask_status;	use et_stencil_mask_status;
with et_stopmask_status;		use et_stopmask_status;
with et_terminal_stopmask;		use et_terminal_stopmask;
with et_terminal_stencil;		use et_terminal_stencil;
with et_terminal_hole;			use et_terminal_hole;
with et_terminal_tht;			use et_terminal_tht;
with et_terminal_name;			use et_terminal_name;
with et_pcb_sides;				use et_pcb_sides;
with et_board_geometry;			use et_board_geometry;
with et_drill;					use et_drill;
with et_design_rules_board;		use et_design_rules_board;



package et_terminal is

	use pac_geometry_brd;
	use pac_geometry_2;
	use pac_contours;

	use pac_basic_objects;
	use pac_basic_objects_circle;


	-- A SMT pad has stopmask on one side only:
	subtype type_stopmask_smt is type_stopmask_shape;






	type type_terminal ( -- CS: should be private
		technology	: type_assembly_technology; -- smt/tht

		 -- drilled/milled, without meaning if technology is SMT
		tht_hole	: type_terminal_tht_hole)
	is new type_object_with_rotation with record

		-- The terminal position (x/y) relative to the package position.
		-- The rotation is the rotation of the terminal about itself:

		-- For SMT pads this should be the geometical center of the pad.
		-- The rotation has no meaning for THT pads with round shape.
		-- The rotation is useful for exotic pad contours. The operator
		-- would be drawing the contour with zero rotation
		-- first (which is easier) in the package model. Then, by applying an angle,
		-- the countour would be rotated to its final position.

		-- CS: When reading a package model, make sure position is inside
		-- the pad shape.

		case technology is
			when THT =>
				-- The shape of the pad on top and bottom side.
				pad_shape_tht			: type_pad_outline_tht;

				stop_mask_status_tht	: type_stop_mask_status := stop_mask_status_default;
				-- CS: The stop mask status applies to both top and bottom of the pad.

				stop_mask_shape_tht		: type_stopmask_tht;

				-- This is the width of the conductor surrounding the
				-- hole in inner layers.
				-- Since the hole can be of any shape we do not speak about "restring".
				-- Nevertheless the type_restring_width is used.
				-- The shape of the conductor area around the hole is the same as
				-- the shape of the hole. No further extra contours possible.
				width_inner_layers	: type_restring_width;

				case tht_hole is
					when DRILLED =>
						-- The hole is just a circular drilling
						-- with its center at position (see above)
						-- and this diameter:
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

				-- In the package model, an SMT pad can be placed
				-- on top or on bottom side:
				face					: type_face;

				stop_mask_status_smt	: type_stop_mask_status := stop_mask_status_default;
				stop_mask_shape_smt		: type_stopmask_smt;

				solder_paste_status		: type_solder_paste_status := solder_paste_status_default;
				stencil_shape			: type_stencil_shape;

		end case;

		-- CS: For the future:
		-- Automatically generated thermal reliefs may not suffice
		-- for large terminals. A flag that determines whether reliefs should be
		-- automatically computed or user defined origins of spokes should be used.
		-- There could be user defined points where the spokes start.

		-- CS thermal_relief on/off ?
	end record;






	-- These subprograms an only applied to terminals
	-- with technology THT, otherwise exeception will be raised:

	procedure set_pad_shape_tht (
		terminal	: in out type_terminal;
		shape		: in type_pad_outline_tht);

	function get_pad_shape_tht (
		terminal	: in type_terminal)
		return type_pad_outline_tht;


	procedure set_stop_mask_status_tht (
		terminal	: in out type_terminal;
		status		: in type_stop_mask_status);

	function get_stop_mask_status_tht (
		terminal	: in type_terminal)
		return type_stop_mask_status;


	procedure set_stop_mask_shape_tht (
		terminal	: in out type_terminal;
		shape		: in type_stopmask_tht);

	function get_stop_mask_shape_tht (
		terminal	: in type_terminal)
		return type_stopmask_tht;


	procedure set_width_inner_layers (
		terminal	: in out type_terminal;
		width		: in type_restring_width);

	function get_width_inner_layers (
		terminal	: in type_terminal)
		return type_restring_width;


	procedure set_drill_size (
		terminal	: in out type_terminal;
		size		: in type_drill_size_tht);

	function get_drill_size (
		terminal	: in type_terminal)
		return type_drill_size_tht;


	procedure set_millings (
		terminal	: in out type_terminal;
		millings	: in type_contour);

	function get_millings (
		terminal	: in type_terminal)
		return type_contour;




	-- These subprograms an only applied to terminals
	-- with technology SMT, otherwise exeception will be raised:

	procedure set_pad_shape_smt (
		terminal	: in out type_terminal;
		shape		: in type_contour);

	function get_pad_shape_smt (
		terminal	: in type_terminal)
		return type_contour;


	procedure set_face (
		terminal	: in out type_terminal;
		face		: in type_face);

	function get_face (
		terminal	: in type_terminal)
		return type_face;


	procedure set_stop_mask_status_smt (
		terminal	: in out type_terminal;
		status		: in type_stop_mask_status);

	function get_stop_mask_status_smt (
		terminal	: in type_terminal)
		return type_stop_mask_status;


	procedure set_stop_mask_shape_smt (
		terminal	: in out type_terminal;
		shape		: in type_stopmask_smt);

	function get_stop_mask_shape_smt (
		terminal	: in type_terminal)
		return type_stopmask_smt;


	procedure set_solder_paste_status (
		terminal	: in out type_terminal;
		status		: in type_solder_paste_status);

	function get_solder_paste_status (
		terminal	: in type_terminal)
		return type_solder_paste_status;


	procedure set_stencil_shape (
		terminal	: in out type_terminal;
		shape		: in type_stencil_shape);

	function get_stencil_shape (
		terminal	: in type_terminal)
		return type_stencil_shape;



	-- For laying out traces we need a type that provides for a terminal information about
	-- x/y/rotation/technology and optionally the face. Face is available if technology is SMT.
	-- NOTE: This type is used in a package model. It uses fixed point numbers for
	-- the terminal positions because the package model is man-made.
	type type_terminal_position (
		technology	: type_assembly_technology)
	-- is new pac_geometry_2.type_position with record
	is new type_object_with_rotation with record
		case technology is
			when SMT => face : type_face;
			when THT => null;
		end case;
	end record;
	-- CS rework ? derive from type_abstract_object ?
	-- already tried: subtype type_terminal_position is type_terminal;
	-- CS: makeshift. leave it as it is ? This was we distiguish
	-- between a terminal and a terminal position.


	-- CS: incomplete !
	function to_position (
		line : in type_fields_of_line; -- "x 23 y 0.2 rotation 90.0 face top"
		from : in type_field_count_positive)
		return type_terminal_position;





	-- NOTE: This type is used in the board when inquiring
	-- for absolute terminal positions.
	-- It uses floating point numbers for the terminal position because:
	-- After rotating the package in the board the x/y coordinates
	-- are machine-made. Fixed point coordinates would not be useful here.
	type type_terminal_position_fine is record
		technology	: type_assembly_technology;

		-- Should be inside the pad. Geometrical center:
		place		: type_vector;
		rotation	: type_angle := 0.0;

		-- This applies for both SMT and THT terminals.
		-- A THT terminal has separate pad shapes for top and bottom.
		face		: type_face;
	end record;



	function to_string (
		position : in type_terminal_position_fine)
		return string;



	-- If a THT-terminal at a particular position has a drilled hole,
	-- then this function should be used to get the outline contour
	-- in inner layers. The outline contour is the area that encircles
	-- the restring. The returned contour is basically a circle:
	function get_inner_contour (
		terminal	: in type_terminal; -- MUST be a THT terminal with drilled hole !
		position	: in type_vector)
		return type_contour;





	-- Logs the properties of the given terminal.
	procedure terminal_properties (
		terminal		: in type_terminal;
		name			: in type_terminal_name;
		log_threshold	: in type_log_level);


end et_terminal;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
