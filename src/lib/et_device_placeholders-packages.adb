------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                   DEVICE PLACEHOLDERS IN PACKAGES                        --
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
--                                                                          --
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


with ada.strings.unbounded;


package body et_device_placeholders.packages is


	function to_placeholder_index (
		index : in string)
		return type_placeholder_index
	is begin
		return type_placeholder_index'value (index);
	end;


	
	
	function locate_placeholder (
		placeholders	: in pac_text_placeholders.vector;
		meaning			: in type_placeholder_meaning;
		index			: in type_placeholder_index)
		return pac_text_placeholders.cursor
	is
		cursor : pac_text_placeholders.cursor;
		
		-- This counter increments each time a placeholder
		-- with the given meaning has been found:
		n : natural := 0;
		
	begin
		-- put_line ("locate_placeholder " & to_string (meaning) & " " 
			-- & type_placeholder_index'image (index));
		
		if not is_empty (placeholders) then
			cursor := placeholders.first;
		end if;
		
	
		-- Iterate though the given placeholders:
		while has_element (cursor) loop
		
			-- Test the meaning of the candidate placeholder:
			if element (cursor).meaning = meaning then
				n := n + 1;
				-- put_line ("n: " & natural'image (n));
		
				-- Abort the iteration once the placeholder
				-- given by index has been found:
				if type_placeholder_index (n) = index then
					-- put_line ("found");
					exit;
				end if;
				
			end if;
		
			next (cursor);
		end loop;
	
		-- If nothing found, then cursor points to no_element.
		
		return cursor;
	end locate_placeholder;
	
	

	

	
	procedure mirror_placeholders (
		placeholders	: in out pac_text_placeholders.vector;
		axis			: in type_mirror := MIRROR_ALONG_Y_AXIS)
	is
		result : pac_text_placeholders.vector;

		procedure query_placeholder (c : in pac_text_placeholders.cursor) is
			ph : type_text_placeholder := element (c);
		begin
			mirror_text (ph, axis);
			result.append (ph);
		end query_placeholder;
		
	begin
		placeholders.iterate (query_placeholder'access);
		placeholders := result;
	end mirror_placeholders;
	
	

	procedure rotate_placeholders (
		placeholders	: in out pac_text_placeholders.vector;
		angle			: in type_rotation_model)
	is
		result : pac_text_placeholders.vector;

		procedure query_placeholder (c : in pac_text_placeholders.cursor) is
			ph : type_text_placeholder := element (c);
		begin
			rotate_text_by (ph, angle);
			result.append (ph);
		end query_placeholder;

	begin
		placeholders.iterate (query_placeholder'access);
		placeholders := result;
	end rotate_placeholders;



	procedure move_placeholders (
		placeholders	: in out pac_text_placeholders.vector;
		offset			: in type_vector_model)
	is
		result : pac_text_placeholders.vector;

		procedure query_placeholder (c : in pac_text_placeholders.cursor) is
			ph : type_text_placeholder := element (c);
		begin
			move_text_to (ph, offset); -- CS should be move_text_by ?
			result.append (ph);
		end query_placeholder;

	begin
		placeholders.iterate (query_placeholder'access);
		placeholders := result;
	end move_placeholders;

	


	procedure placeholder_properties (
		face			: in type_face;
		cursor			: in pac_text_placeholders.cursor;
		log_threshold 	: in type_log_level) 
	is
		use pac_text_placeholders;
		placeholder : type_text_placeholder renames element (cursor);
	begin
		log (text => "placeholder face" & to_string (face)
			 & " for " & to_string (placeholder.meaning), level => log_threshold);
		
		log_indentation_up;
		log (text => text_properties (type_text (placeholder)), level => log_threshold + 1);
		log_indentation_down;
	end placeholder_properties;




	

	function to_string (
		layer : in type_placeholder_layer)
		return string 
	is begin
		return to_lower (type_placeholder_layer'image (layer));
	end;

	
	function to_placeholder_layer (
		layer : in string) 
		return type_placeholder_layer 
	is begin
		return type_placeholder_layer'value (layer);
	end;


	
	
	
	
	
	
	function to_string (
		placeholders : in type_text_placeholders)
		return string
	is
		use ada.strings.unbounded;
		result : unbounded_string;

		
		procedure query_placeholder (c : in pac_text_placeholders.cursor) is
			p : type_text_placeholder renames element (c);
		begin
			result := result & " meaning " & to_string (p.meaning) 
				& " place " & to_string (get_place (p));
		end query_placeholder;
		
		 
	begin
		result := result & " silkscreen top: ";
		placeholders.silkscreen.top.iterate (query_placeholder'access);

		result := result & " silkscreen bottom: ";
		placeholders.silkscreen.bottom.iterate (query_placeholder'access);

		-- CS assy doc
		
		return to_string (result);
	end to_string;
	

	
	

	

	procedure move_placeholder (
		placeholders	: in out type_text_placeholders;
		meaning			: in type_placeholder_meaning;					 
		layer			: in type_placeholder_layer; -- silkscreen, assy doc
		face			: in type_face;
		index			: in type_placeholder_index; -- 1, 2, 3, ...
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_vector_model)
	is
		-- The addressed placeholder must be located among the given
		-- placeholders. If the placeholder exists, then this cursor
		-- will be pointing to it:
		cursor : pac_text_placeholders.cursor;

		
		procedure query_placeholder (
			p : in out type_text_placeholder)
		is begin
			case coordinates is
				when ABSOLUTE =>
					-- put_line ("move absolute");
					move_text_to (p, point);
					
					-- put_line ("new " & to_string (get_place (p)));
					
				when RELATIVE =>
					-- put_line ("move relative " & to_string (point));
					move_text_by (p, point);
					
					-- put_line ("new " & to_string (get_place (p)));
			end case;
		end query_placeholder;
		
		
		
		procedure do_silkscreen is begin
			-- put_line ("do_silkscreen");
			
			case face is
				when TOP => 
					cursor := locate_placeholder (placeholders.silkscreen.top, meaning, index);

					-- If the specified placeholder exists, then do the actual move:
					if has_element (cursor) then
						-- put_line ("placeholder in silkscreen found");
						placeholders.silkscreen.top.update_element (cursor, query_placeholder'access);
					end if;

					
				when BOTTOM	=>
					cursor := locate_placeholder (placeholders.silkscreen.bottom, meaning, index);

					-- If the specified placeholder exists, then do the actual move:
					if has_element (cursor) then
						placeholders.silkscreen.bottom.update_element (cursor, query_placeholder'access);
					end if;
				
			end case;
		end do_silkscreen;
		
		
		
		procedure do_assy_doc is begin
			-- put_line ("do_assy_doc");
		
			case face is
				when TOP => 
					cursor := locate_placeholder (placeholders.assy_doc.top, meaning, index);
		
					-- If the specified placeholder exists, then do the actual move:
					if has_element (cursor) then
						placeholders.assy_doc.top.update_element (cursor, query_placeholder'access);
					end if;

		
				when BOTTOM	=> 
					cursor := locate_placeholder (placeholders.assy_doc.bottom, meaning, index);

					-- If the specified placeholder exists, then do the actual move:
					if has_element (cursor) then
						placeholders.assy_doc.bottom.update_element (cursor, query_placeholder'access);
					end if;

			end case;		
		end do_assy_doc;
		
		
		
	begin
		-- put_line ("move_placeholder");
		
		case layer is
			when SILKSCREEN =>
				do_silkscreen;
					
			when ASSY_DOC =>
				do_assy_doc;
		end case;		
		
		-- put_line (to_string (placeholders));
	end move_placeholder;


	
	
	
	
	

	procedure rotate_placeholder (
		placeholders	: in out type_text_placeholders;
		meaning			: in type_placeholder_meaning;					 
		layer			: in type_placeholder_layer; -- silkscreen, assy doc
		face			: in type_face;
		index			: in type_placeholder_index; -- 1, 2, 3, ...
		coordinates		: in type_coordinates;
		rotation		: in type_rotation_model)
	is
	begin
		null;
		-- CS do similar as in move_placeholder above.
	end rotate_placeholder;


	
	
	
	
end et_device_placeholders.packages;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
