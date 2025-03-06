------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         LAYER DISPLAY BOARD                              --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                -- 
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

with et_pcb_sides;				use et_pcb_sides;
with et_pcb_coordinates_2;		use et_pcb_coordinates_2;
with et_pcb_stack;				use et_pcb_stack;

package et_display.board is

	type type_paired is record
		top, bottom : type_layer_status := OFF;
	end record;

	type type_conductors is array (type_signal_layer'first .. type_signal_layer'last) 
		of type_layer_status;

-- 	type type_vias is array (type_signal_layer'first .. type_signal_layer'last) 
-- 		of type_layer_status;
		
	type type_route_restrict is array (type_signal_layer'first .. type_signal_layer'last) 
		of type_layer_status;

	type type_via_restrict is array (type_signal_layer'first .. type_signal_layer'last) 
		of type_layer_status;
	

	
	type type_layers is record
		outline			: type_layer_status := ON;
		plated_millings	: type_layer_status := ON;
		
		silkscreen		: type_paired := (top => ON, bottom => OFF);
		assy_doc		: type_paired := (top => ON, bottom => OFF);
		keepout			: type_paired := (others => OFF);
		stop_mask		: type_paired := (others => OFF); -- CS rename to stopmask
		stencil			: type_paired := (others => OFF);
		device_origins	: type_paired := (others => ON);
		-- CS text_origins (for texts and placeholders) ?

		ratsnest		: type_layer_status := ON;

		
		-- By default conductor layers 1..4 are displayed:
		conductors		: type_conductors := (
							1 => ON,
							2 => ON,
							3 => ON,
							4 => ON,
							others => OFF);
		
		--vias			: type_vias := (others => OFF);
		vias			: type_layer_status := ON;
		route_restrict	: type_route_restrict := (others => OFF);
		via_restrict	: type_via_restrict := (others => OFF);		
	end record;

	layers : type_layers;



-- BOARD OUTLINE:

	function board_contour_enabled return boolean;

	procedure enable_board_contour;

	procedure disable_board_contour;


	

-- PLATED MILLINGS:
	
	function plated_millings_enabled return boolean;


	

	
-- SILKSCREEN:
	
	function silkscreen_enabled (
		face : in type_face) 
		return boolean;

	procedure enable_silkscreen (
		face : in type_face);



	
-- ASSEMBLY DOCUMENTATION:
		
	function assy_doc_enabled (
		face : in type_face) 
		return boolean;	


	procedure enable_assy_doc (
		face : in type_face);

	

-- KEEPOUT:
	
	function keepout_enabled (
		face : in type_face)
		return boolean;

	
	procedure enable_keepout (
		face : in type_face);	

	


-- STOPMASK:
	
	function stop_mask_enabled (
		face : in type_face) 
		return boolean;


	procedure enable_stopmask (
		face : in type_face);	


	


-- STENCIL:
	
	function stencil_enabled (
		face : in type_face) 
		return boolean;


	procedure enable_stencil (
		face : in type_face);	

	


	
	function device_origins_enabled (face : in type_face) return boolean;

	function ratsnest_enabled return boolean;




	
-- CONDUCTOR LAYERS
	
	-- Returns true if any conductor layer is enabled:
	function conductors_enabled return boolean;

	
	-- Returns true if any inner conductor layer is enabled:
	function inner_conductors_enabled (
		deepest_layer : in type_signal_layer) -- the deepest conductor layer of the board
		return boolean;

	
	-- Returns true if the given conductor layer is enabled:
	function conductor_enabled (
		layer : in type_signal_layer) return boolean;

	
	-- Enables the given conductor layer:
	procedure enable_conductor (
		layer : in type_signal_layer);

	
	-- Returns true if via layer is enabled:
	function vias_enabled return boolean;



	

	
	
-- ROUTE RESTRICT

	-- Returns true if any route restrict layer is enabled:
	function route_restrict_enabled return boolean;

	
	-- Returns true if the given route restrict layer is enabled:
	function route_restrict_layer_enabled (
		layer : in type_signal_layer) 
		return boolean;

	
	-- Returns true if at least one of the given route restrict layers is enabled.
	function route_restrict_layer_enabled (
		layers : in type_signal_layers.set)
		return boolean;

	
	-- Returns true if the route restrict layer 
	-- on TOP/BOTTOM is enabled:
	function route_restrict_enabled (
		face 			: in type_face;
		deepest_layer	: in type_signal_layer) -- the deepest conductor layer of the board
		return boolean;


	-- Enables the given route restrict layer:
	procedure enable_route_restrict (
		layer : in type_signal_layer);

	
	

-- VIA RESTRICT

	-- Returns true if any via restrict layer is enabled:
	function via_restrict_enabled return boolean;	

	
	-- Returns true if the given via restrict layer is enabled:
	function via_restrict_layer_enabled (
		layer : in type_signal_layer) 
		return boolean;

	
	-- Returns true if at least one of the given via restrict layers is enabled.
	function via_restrict_layer_enabled (
		layers : in type_signal_layers.set)
		return boolean;

	
	-- Returns true if the via restrict layer 
	-- on TOP/BOTTOM is enabled:
	function via_restrict_enabled (
		face 			: in type_face;
		deepest_layer	: in type_signal_layer) -- the deepest conductor layer of the board
		return boolean;

	
	-- Enables the given via restrict layer:
	procedure enable_via_restrict (
		layer : in type_signal_layer);

	
	
	-- Returns all enabled conductor layers in a string like "1..4,7,10..32"
	function enabled_conductor_layers return string;
	
end et_display.board;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
