------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                               VIAS                                       --
--                                                                          --
--                              B o d y                                     --
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

with ada.strings;					use ada.strings;

with et_exceptions;					use et_exceptions;

package body et_vias is
	

	function to_micro_vias_allowed (allowed : in string) return type_micro_vias_allowed is begin
		return type_micro_vias_allowed'value (allowed);
	end to_micro_vias_allowed;
	
	function to_string (allowed : in type_micro_vias_allowed) return string is begin
		return " micro vias allowed " & type_micro_vias_allowed'image (allowed);
	end to_string;


	function to_string (category : in type_via_category) return string is begin
		return to_lower (type_via_category'image (category));
	end to_string;

	function to_via_category (category : in string) return type_via_category is begin
		return type_via_category'value (category);
	end to_via_category;


	function buried_via_uses_layer (
		via		: in type_via;
		layer	: in type_signal_layer)
		return boolean
	is
		-- if the given via is not buried, then an exception is raised here,
		-- because only buried vias have the element "layers":
		subtype stack is type_via_layer range via.layers.upper .. via.layers.lower;
	begin
		if layer in stack then
			return true;
		else
			return false;
		end if;
	end buried_via_uses_layer;


	function blind_via_uses_layer (
		via		: in type_via;
		layer	: in type_signal_layer;
		bottom	: in type_signal_layer := type_signal_layer'last)
		return boolean
	is begin
		case via.category is
			when BLIND_DRILLED_FROM_TOP =>
				declare
					subtype stack is type_via_layer 
						range type_signal_layer'first + 1 .. via.lower;
				begin
					if layer in stack then
						return true;
					else
						return false;
					end if;
				end;
				
			when BLIND_DRILLED_FROM_BOTTOM =>
				declare
					subtype stack is type_via_layer 
						range via.upper .. bottom - 1;
				begin
					if layer in stack then
						return true;
					else
						return false;
					end if;
				end;
				
			when others => raise constraint_error;
		end case;
	end blind_via_uses_layer;


	procedure iterate (
		vias	: in pac_vias.list;
		process	: not null access procedure (position : in pac_vias.cursor);
		proceed	: not null access boolean)
	is
		use pac_vias;
		c : pac_vias.cursor := vias.first;
	begin
		while c /= pac_vias.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;


	
	function to_string (via : in type_via) return string is
		
		function get_misc return string is begin
			case via.category is
				when THROUGH =>
					return ""; -- nothing special
				
				when BLIND_DRILLED_FROM_TOP =>
					return " / restr. top:" & to_string (via.restring_top)
						& " / deepest layer:" & to_string (via.lower);
					
				when BLIND_DRILLED_FROM_BOTTOM =>
					return " / restr. bottom:" & to_string (via.restring_bottom)
						& " / hightest layer:" & to_string (via.upper);

				when BURIED =>
					return " / layers:" & to_string (via.layers);
			end case;
		end get_misc;

	begin
		return "via: " & to_string (type_drill (via)) 
			& " / CAT: " & to_string (via.category)
			& " / restr. inner: " & to_string (via.restring_inner)
			& get_misc;
	end to_string;
	
	
	function to_buried_layers (
		upper, lower	: in string; -- 2, 6
		bottom			: in type_signal_layer) -- 16
		return type_buried_layers
	is
		u, l : type_signal_layer;
		layers : type_buried_layers; -- to be returned

		text : constant string := " invalid. Must be an inner signal layer !";
	begin
		u := to_signal_layer (upper);
		l := to_signal_layer (lower);

		-- Validate upper layer:
		if u > type_signal_layer'first and u < bottom then
			layers.upper := u;
		else
			raise semantic_error_1 with
				"ERROR: Layer " & upper & text;
		end if;

		-- Validate lower layer:
		if u < l then
			if l < bottom then
				layers.lower := l;
			else
				raise semantic_error_1 with
					"ERROR: Layer " & lower & text;
			end if;
		else
			raise semantic_error_1 with
				"ERROR: The lower layer must be deeper than " & upper & " !";
		end if;
		
		return layers;
	end to_buried_layers;

	
	function to_string (layers : in type_buried_layers) return string is
	begin
		return to_string (layers.upper) & space & to_string (layers.lower);
	end to_string;

	
	function to_polygon (
		position	: in type_point;
		restring	: in type_restring_width;
		diameter	: in type_drill_size;
		tolerance	: in type_distance_positive)
		return type_polygon
	is 
		use pac_geometry_brd;
		use et_contour_to_polygon;
	begin
		return (
			edges => (to_edges (
				circle		=> (position, type_float_internal_positive (restring + diameter * 0.5)),
				mode		=> EXPAND,				   
				tolerance	=> tolerance)),

			others => <>); -- boundaries not computed here

	end to_polygon;

	
end et_vias;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
