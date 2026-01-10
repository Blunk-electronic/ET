------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                           PACKAGE MODEL                                  --
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


with et_mirroring;				use et_mirroring;
with et_pcb_sides;				use et_pcb_sides;
with et_board_geometry;			use et_board_geometry;
with et_board_coordinates;		use et_board_coordinates;
with et_pcb_stack;				use et_pcb_stack;
with et_board_text;				use et_board_text;
with et_drills;					use et_drills;
with et_assembly_technology;	use et_assembly_technology;
with et_terminals;				use et_terminals;
with et_text;
with et_design_rules_board;				use et_design_rules_board;
with et_route_restrict.packages;		use et_route_restrict.packages;
with et_via_restrict.packages;			use et_via_restrict.packages;
with et_stopmask.packages;				use et_stopmask.packages;
with et_stencil.packages;				use et_stencil.packages;
with et_silkscreen;						use et_silkscreen;
with et_silkscreen.packages;			use et_silkscreen.packages;
with et_assy_doc.packages;				use et_assy_doc.packages;
with et_keepout;						use et_keepout;
with et_keepout.packages;				use et_keepout.packages;
with et_board_holes;					use et_board_holes;
with et_package_bom_relevance;			use et_package_bom_relevance;
with et_package_name;					use et_package_name;
with et_package_description;			use et_package_description;
with et_device_placeholders.packages;	use et_device_placeholders.packages;
with et_conductors_floating_package;	use et_conductors_floating_package;

with et_logging;						use et_logging;


package et_package_model is
	
	use pac_geometry_brd;
	use pac_geometry_2;
	use pac_contours;

	

	
-- PLACEHOLDERS FOR TEXTS
	--type type_text_meaning_package is (NAME, VALUE, PURPOSE);

	--function to_string (text_meaning : in type_text_meaning_package) return string;
	--function to_text_meaning (text_meaning : in string) return type_text_meaning_package;
	
	--type type_text_placeholder is new type_text_fab with record
		--meaning : type_text_meaning_package := NAME;
	--end record;

	---- There can be lots of placeholders of this kind. So they are stored in a list:	
	--package pac_text_placeholders is new doubly_linked_lists (type_text_placeholder);

	---- Placeholders for device name and value can be placed in
	---- silk screen or assembly documentation only:
	--type type_placeholder_package_layer is (SILK_SCREEN, ASSEMBLY_DOCUMENTATION);
	--function to_string (layer : in type_placeholder_package_layer) return string;
	--function to_layer (layer : in string) return type_placeholder_package_layer;
	
	---- A collection of text placeholders in silk screen and assembly documentation 
	---- modelled by this type. The user is free to change them in the 
	---- layout (position, text size, rotation, line width ...).
	---- Initally, when a device is added to the schematic, these placeholders are 
	---- copies of the placeholders defined in the package model.
	--type type_text_placeholders_silk_screen is record
		--top		: pac_text_placeholders.list;
		--bottom	: pac_text_placeholders.list;
	--end record;

	--type type_text_placeholders_assembly_documentation is record
		--top		: pac_text_placeholders.list;
		--bottom	: pac_text_placeholders.list;
	--end record;

	--type type_text_placeholders is record
		--silk_screen	: type_text_placeholders_silk_screen;
		--assy_doc	: type_text_placeholders_assembly_documentation;
	--end record;

	

	
	---- Silkscreen objects include placeholders for device name,
	---- value, purpose:
	--type type_silkscreen_package is new type_silkscreen with record
		--placeholders : pac_text_placeholders.list;
	--end record;

	
	---- Silkscreen is about two sides of the board:
	--type type_silkscreen_both_sides is record
		--top		: type_silkscreen_package;
		--bottom	: type_silkscreen_package;
	--end record;

	

	-- Assembly documentation includes placeholders:
	--type type_assembly_documentation 
		--is new et_assy_doc.packages.type_assembly_documentation with 
	--record
		--placeholders: pac_text_placeholders.list;
	--end record;

	---- Because assembly documentation is about two sides of the board this composite is required:
	--type type_assembly_documentation_both_sides is record
		--top		: type_assembly_documentation;
		--bottom	: type_assembly_documentation;
	--end record;


	
	-- This is the base type of a package:
	type type_package_base (appearance : type_bom_relevant) is abstract tagged record
		description		: pac_package_description.bounded_string;
		conductors		: type_conductor_objects_both_sides; -- non-electric objects
		keepout 		: type_keepout_both_sides;
		stop_mask		: type_stopmask_both_sides; -- not terminal related -- CS rename to stopmask
		stencil			: type_stencil_both_sides; -- not terminal related
		route_restrict 	: type_route_restrict;
		via_restrict 	: type_via_restrict;
		holes			: pac_holes.list; -- PCB contour
		-- CS plated holes ?
		
		-- NOTE: There is no reason to allow texts in contours here.
		-- The text would most likely end up somewhere inside the board area. 
		-- This in turn would cause the DRC to output errors.
		
		technology		: type_assembly_technology := SMT; -- set by majority of terminals
		
		case appearance is
			when BOM_RELEVANT_YES =>				
				null; -- CS
				
			when BOM_RELEVANT_NO =>
				null; -- fiducials, testpoints, board edge connectors, ...
		end case;
	end record;


	-- origin_half_size : constant type_distance_positive := 1.0;
	-- origin_line_width : constant type_distance_positive := 0.01;
	-- CS no need. as we use a standard origin (see package et_canvas)

	
	-- A package in the library extends the base package type:
	type type_package_model is new type_package_base with record
		-- CS default for face ?
		silkscreen	: type_silkscreen_both_sides; -- incl. placeholder for name and purpose
		assy_doc	: type_assy_doc_both_sides; -- incl. placeholder for value
		terminals	: pac_terminals.map;
	end record;



	type type_package_model_access is access type_package_model;

	
	

	-- Returns true if the given package is
	-- a real package with a height, means if it is relevant 
	-- for creating bill of materials (BOM):
	function is_bom_relevant (
		packge : in type_package_model)
		return boolean;




	-- Returns the default placeholders of the package
	-- as they are specified in the package model:
	function get_default_placeholders (
		packge : in type_package_model)
		return type_text_placeholders;

	


	-- Rotates/mirrors/moves the contours of stop mask, stencil, holes, ...
	-- of a terminal according to the position and flip status of a package:
	procedure move_contours (
		term_pos	: in out type_position; -- terminal position
		outline		: in out type_contour'class;	-- contours of terminal (smt or tht)
		flipped		: in type_flipped;		-- package flip status
		package_pos	: in type_package_position); -- package position
		

	
	

-- PROPERTIES OF OBJECTS IN BOARD CONTOUR / OUTLINE / EDGE CUTS
	--procedure line_pcb_contour_properties (
	---- Logs the properties of the given line of pcb contour
		--cursor			: in pac_pcb_contour_lines.cursor;
		--log_threshold 	: in type_log_level);

	--procedure arc_pcb_contour_properties (
	---- Logs the properties of the given arc of pcb contour
		--cursor			: in pac_pcb_contour_arcs.cursor;
		--log_threshold 	: in type_log_level);

	--procedure circle_pcb_contour_properties (
	---- Logs the properties of the given circle of pcb contour
		--cursor			: in pac_pcb_contour_circles.cursor;
		--log_threshold 	: in type_log_level);

	

	
end et_package_model;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
