------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          PCB / SIGNAL LAYERS                             --
--                                                                          --
--                               B o d y                                    --
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

with ada.text_io;				use ada.text_io;
with ada.strings;				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.exceptions;

with et_string_processing;		use et_string_processing;


package body et_pcb_signal_layers is


	function to_string (
		category	: in type_signal_layer_category)
		return string
	is begin
		return type_signal_layer_category'image (category);
	end;


	
		
	function invert_category (cat : in type_signal_layer_category)
		return type_signal_layer_category
	is 
		result : type_signal_layer_category := INNER;
	begin
		case cat is
			when OUTER_TOP		=> result := OUTER_BOTTOM;
			when OUTER_BOTTOM	=> result := OUTER_TOP;
			when INNER			=> null; -- assume default
		end case;

		return result;
	end invert_category;


	
	
	function to_string (layer : in type_signal_layer) return string is begin
		return trim (type_signal_layer'image (layer), left);
	end to_string;

	
	
	function to_signal_layer (layer : in string) return type_signal_layer is begin
		return type_signal_layer'value (layer);
	end to_signal_layer;


	
	
	function to_string (layers : in pac_signal_layers.set) return string is
	-- Returns a string like '[1,3,5-9]'.
	-- CS: Currently the range notation like 5-9 is not supported. The return is 5,6,7,8,9 instead.
		

		-- The layer numbers will be stored here:
		package type_layer_string is new generic_bounded_length (100); -- CS increase if necessary.
		use type_layer_string;
		layer_string : type_layer_string.bounded_string; -- to be returned

		-- set a cursor to the last layer in the given layer set:
		last_layer : pac_signal_layers.cursor := layers.last;
		
		
		procedure query_layer (cursor : in pac_signal_layers.cursor) is begin
		-- Append the layer number to the return string.
			layer_string := layer_string & to_bounded_string (to_string (element (cursor)));

			-- If not the last layer, append a comma:
			if cursor /= last_layer then
				layer_string := layer_string & layer_term_separator;
			end if;
		end;

	begin -- to_string
		-- the return string always starts with an opening bracket:
		layer_string := layer_string & layer_term_start;
		
		iterate (layers, query_layer'access);

-- 		for l in type_signal_layer'first .. type_signal_layer'last loop
-- 			null;
-- 		end loop;

		-- the return string always ends with a closing bracket:
		layer_string := layer_string & layer_term_end;
		
		return to_string (layer_string);
	end;


	



	
	
	
	function to_layers (layers : in string) 
		return pac_signal_layers.set 
	is
		layer_set : pac_signal_layers.set; -- to be returned
		char : character;

		package number_string is new generic_bounded_length (3);
		use number_string;
		number : number_string.bounded_string;

		
		procedure reset_number is begin 
			number := to_bounded_string ("");
		end;

		range_started : boolean := false;
		range_start, range_end : type_signal_layer;

		
		procedure warning (layer : in type_signal_layer) is begin
			log (WARNING, "Multiple occurence of layer " & to_string (layer) & " !");
		end;

		
		procedure insert_layer is 
			l : type_signal_layer := to_signal_layer (to_string (number));
		begin
			if not contains (layer_set, l) then
				insert (layer_set, l);
			else
				warning (l);
			end if;
		end;

		
		procedure insert_range is 
			inserted : boolean := false;
		begin
			-- The range must be going upwards:
			if range_start > range_end then
				log (ERROR, "Layer range must be going upwards !", console => true);
				raise constraint_error;
			else
				for l in range_start .. range_end loop

					if not contains (layer_set, l) then
						insert (layer_set, l);
					else
						warning (l);
					end if;
				end loop;
			end if;
		end;

		
	begin -- to_layers
		if layers'length = 0 then
			log (ERROR, "Layer term must not be empty !", console => true);
			raise constraint_error;
		end if;
		
		for place in layers'first .. layers'last loop
			char := layers (place);

			if place = layers'first and char /= layer_term_start then
				log (ERROR, "Layer term must start with '" & layer_term_start & "' !", console => true);
				raise constraint_error;
				
			elsif place = layers'last and char /= layer_term_end then
				log (ERROR, "Layer term must end with '" & layer_term_end & "' !", console => true);
				raise constraint_error;

			elsif char = layer_term_start then null;
				
			elsif is_digit (char) then
				number := number & char;

			elsif char = layer_term_separator then

				if range_started then
					range_end := to_signal_layer (to_string (number));
					range_started := false;
					insert_range;
				else
					insert_layer;
				end if;

				reset_number;

			elsif char = layer_term_end then

				if range_started then
					range_end := to_signal_layer (to_string (number));
					range_started := false;
					insert_range;
				else
					insert_layer;
				end if;
				
			elsif char = layer_term_range then
				range_started := true;
				range_start := to_signal_layer (to_string (number));
				reset_number;
				
			else
				log (ERROR, "Invalid character in layer term !", console => true);
				raise constraint_error;
			end if;
		end loop;
		
		return layer_set;
	end to_layers;



	

	function to_layers (
		layer	: in type_signal_layer)
		return pac_signal_layers.set
	is
		result : pac_signal_layers.set;
	begin
		result.insert (layer);
		return result;
	end to_layers;


	
	
	procedure mirror_signal_layers (
		signal_layers	: in out pac_signal_layers.set;
		deepest_layer	: in type_signal_layer) 
	is
		mir : pac_signal_layers.set;

		procedure query_layer (l : in pac_signal_layers.cursor) is begin
			mir.insert (1 + deepest_layer - element (l));
		end query_layer;
		
	begin
		-- query layers one by one and insert the mirrored layer in mir:
		signal_layers.iterate (query_layer'access);

		-- overwrite given layers by mirrored layer set:
		signal_layers := mir;
	end mirror_signal_layers;

	
end et_pcb_signal_layers;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
