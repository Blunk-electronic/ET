------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                               VIAS                                       --
--                                                                          --
--                              B o d y                                     --
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

with ada.text_io;					use ada.text_io;
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
		return type_via_category'image (category);
	end to_string;

	
	function to_via_category (category : in string) return type_via_category is begin
		return type_via_category'value (category);
	end to_via_category;

	

	function get_bounding_box (
		via : in type_via)
		return type_area
	is
		b : type_area;
		circle : type_circle;
		restring : type_distance_positive;
	begin
		case via.category is
			when THROUGH =>
				restring := get_greatest (via.restring_inner, via.restring_outer);

			when BLIND_DRILLED_FROM_TOP =>
				restring := get_greatest (via.restring_inner, via.restring_top);

			when BLIND_DRILLED_FROM_BOTTOM =>
				restring := get_greatest (via.restring_inner, via.restring_bottom);

			when BURIED =>
				restring := via.restring_inner;
		end case;
				
		set_center (circle, via.position);
		set_radius (circle, 0.5 * via.diameter + restring);
		
		b := get_bounding_box (circle => circle, width => 0.0);
		return b;
	end get_bounding_box;



	
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


	
	function to_string (
		via : in pac_vias.cursor) 
		return string
	is begin
		return to_string (element (via).position);
	end to_string;
		

	
	function get_position (
		via : in pac_vias.cursor)
		return type_vector_model
	is begin
		return get_position (element (via));
	end get_position;


	

	function is_selected (
		via : in pac_vias.cursor)
		return boolean
	is begin
		return is_selected (element (via));
	end;

	
	function is_proposed (
		via : in pac_vias.cursor)
		return boolean
	is begin
		return is_proposed (element (via));
	end;


	function is_moving (
		via : in pac_vias.cursor)
		return boolean
	is begin
		return is_moving (element (via));
	end;

	

	
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
		position	: in type_vector_model;
		restring	: in type_restring_width;
		diameter	: in type_drill_size;
		tolerance	: in type_distance_positive)
		return type_polygon
	is 
		use pac_geometry_brd;
		use et_contour_to_polygon;
	begin
		return optimize_edges ((
			edges => (to_edges (
				circle		=> type_circle (to_circle (position, restring + diameter * 0.5)),
				mode		=> EXPAND,				   
				tolerance	=> tolerance)),

			others => <>)); -- boundaries not computed here

	end to_polygon;

	
end et_vias;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
