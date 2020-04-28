------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         LAYER DISPLAY BOARD                              --
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

with et_pcb_coordinates;		use et_pcb_coordinates;
with et_pcb_stack;				use et_pcb_stack;

package et_display.board is

	type type_paired is record
		top, bottom : type_layer_status := OFF;
	end record;

	type type_conductors is array (type_signal_layer'first .. type_signal_layer'last) 
		of type_layer_status;

	type type_vias is array (type_signal_layer'first .. type_signal_layer'last) 
		of type_layer_status;
		
	type type_route_restrict is array (type_signal_layer'first .. type_signal_layer'last) 
		of type_layer_status;

	type type_via_restrict is array (type_signal_layer'first .. type_signal_layer'last) 
		of type_layer_status;
	
	keyword_route	: constant string := "route";
	keyword_via		: constant string := "via";
		
	type type_layers is record
		outline			: type_layer_status := ON;
		plated_millings	: type_layer_status := ON;
		
		silkscreen	: type_paired := (top => ON, bottom => OFF);
		assy_doc	: type_paired := (top => ON, bottom => OFF);
		keepout		: type_paired := (others => OFF);
		stop_mask	: type_paired := (others => OFF);
		stencil		: type_paired := (others => OFF);

		conductors		: type_conductors := (type_conductors'first => ON, others => OFF);
		vias			: type_vias := (others => OFF);
		route_restrict	: type_route_restrict := (others => OFF);
		via_restrict	: type_via_restrict := (others => OFF);		
	end record;

	layers : type_layers;

	function outline_enabled return boolean;
	function plated_millings_enabled return boolean;
	
	function silkscreen_enabled (face : in type_face) return boolean;
	function assy_doc_enabled (face : in type_face) return boolean;	
	function keepout_enabled (face : in type_face) return boolean;
	function stop_mask_enabled (face : in type_face) return boolean;
	function stencil_enabled (face : in type_face) return boolean;

	-- Returns true if any conductor layer is enabled:
	function conductors_enabled return boolean;

	-- Returns true if the given conductor layer is enabled:
	function conductor_enabled (layer : in type_signal_layer) return boolean;

	-- Returns true if any route restrict layer is enabled:
	function route_restrict_enabled return boolean;

	-- Returns true if any via restrict layer is enabled:
	function via_restrict_enabled return boolean;	

	-- Returns true if at least one of the given route restrict layers is enabled.
	function route_restrict_layer_enabled (layers : in type_signal_layers.set)
		return boolean;

	-- Returns true if at least one of the given via restrict layers is enabled.
	function via_restrict_layer_enabled (layers : in type_signal_layers.set)
		return boolean;
	
	-- Returns all enabled conductor layers:
	function enabled_conductor_layers return string;
	
end et_display.board;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
