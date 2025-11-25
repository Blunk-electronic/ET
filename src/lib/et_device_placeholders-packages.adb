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

with et_display.board;



package body et_device_placeholders.packages is




	procedure set_anchor_mode (
		placeholder		: in out type_text_placeholder;
		mode			: in type_anchor_mode)
	is begin
		placeholder.anchor_mode := mode;
	end;
	
		

	function get_anchor_mode (
		placeholder		: in type_text_placeholder)
		return type_anchor_mode
	is begin
		return placeholder.anchor_mode;
	end;






	function get_absolute_position (
		placeholder			: in type_text_placeholder;
		package_position	: in type_package_position)
		return type_vector_model
	is 
		result : type_vector_model;
		
		placeholder_place : type_vector_model;
		
		package_place : type_vector_model := get_place (package_position);
		
	begin
		placeholder_place := get_place (placeholder);
		
		move_by (placeholder_place, package_place);
		
		return placeholder_place;
	end;

	
	



	function get_meaning (
		placeholder : in type_text_placeholder)
		return type_placeholder_meaning
	is begin
		return placeholder.meaning;
	end;


	
	
	function to_string (
		placeholder : in type_text_placeholder)
		return string
	is begin
		return "meaning " & to_string (placeholder.meaning)
			& " place " & to_string (get_place (placeholder))
			& " rotation " & to_string (get_rotation (placeholder));
	end;
	


	function to_placeholder_index (
		index : in string)
		return type_placeholder_index
	is begin
		return type_placeholder_index'value (index);
	end;


	

	
	
	procedure iterate (
		placeholders	: in pac_text_placeholders.vector;
		process			: not null access procedure (position : in pac_text_placeholders.cursor);
		proceed			: not null access boolean)
	is
		c : pac_text_placeholders.cursor := placeholders.first;
	begin
		while c /= pac_text_placeholders.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;

	
	
	
	
	
	function get_meaning (
		placeholder : in pac_text_placeholders.cursor)
		return type_placeholder_meaning
	is begin
		return element (placeholder).meaning;
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
			result := result & to_string (p);
		end query_placeholder;
		
		 
	begin
		result := result & " silkscreen top: ";
		placeholders.silkscreen.top.iterate (query_placeholder'access);

		result := result & " silkscreen bottom: ";
		placeholders.silkscreen.bottom.iterate (query_placeholder'access);

		result := result & " assy_doc top: ";
		placeholders.assy_doc.top.iterate (query_placeholder'access);

		result := result & " assy_doc bottom: ";
		placeholders.assy_doc.bottom.iterate (query_placeholder'access);
		
		return to_string (result);
	end to_string;
	

	

	
	

	

	procedure move_placeholder (
		placeholders		: in out type_text_placeholders;
		meaning				: in type_placeholder_meaning;					 
		layer				: in type_placeholder_layer; -- silkscreen, assy doc
		face				: in type_face;
		index				: in type_placeholder_index; -- 1, 2, 3, ...
		package_position	: in type_package_position;
		coordinates			: in type_coordinates; -- relative/absolute
		point				: in type_vector_model)
	is
		-- The addressed placeholder must be located among the given
		-- placeholders. If the placeholder exists, then this cursor
		-- will be pointing to it:
		cursor : pac_text_placeholders.cursor;

		
		procedure query_placeholder (
			p : in out type_text_placeholder)
		is 
			

			-- This procedure switches the anchor mode
			-- and assigns the given relative position to
			-- the given placeholder:
			procedure move_relative (offset : in type_vector_model) is
				-- tp_1, tp_2 : type_vector_model;
				-- a : type_rotation_model := get_rotation (package_position);
				-- pp : type_vector_model := get_place (package_position);
			begin
				-- put_line ("move relative " & to_string (point));

				set_anchor_mode (p, ANCHOR_MODE_1);
				set_place (p, point);
				
-- 				CS: This is experimental stuff. It moves the placeholder
--				independend of the rotation of the package to the
--				given relative position. If this stuff is not required
--				anymore then delete the declarations (above) and the parameter
--				"offset" of this procedure.
--
-- 				-- Get the current absolute position:
-- 				tp_1 := get_absolute_position (p, package_position);
-- 				-- put_line ("tp1 " & to_string (tp_1));
-- 				
-- 				tp_2 := tp_1;
-- 				move_by (tp_2, offset);
-- 				-- put_line ("tp2 " & to_string (tp_2));
-- 				
-- 				rotate_by (tp_2, -a, tp_1);
-- 				-- put_line ("tp2 " & to_string (tp_2));
-- 				
-- 				-- Get the new relative position:
-- 				move_by (tp_2, invert (pp));
-- 				-- put_line ("tp2 " & to_string (tp_2));
-- 				set_place (p, tp_2);
				
				-- put_line ("new " & to_string (get_place (p)));
			end move_relative;
			

			
			-- This procedure switches the anchor mode
			-- and assigns the given absolute position to
			-- the given placeholder:
			procedure move_absolute is begin
				-- put_line ("move absolute");
				
				set_anchor_mode (p, ANCHOR_MODE_2);
				set_place (p, point);
				
				-- put_line ("new " & to_string (get_place (p)));
			end move_absolute;
			
			
		begin		
			case coordinates is
				when RELATIVE =>
					move_relative (point);
					
				when ABSOLUTE =>
					move_absolute;

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
		-- The addressed placeholder must be located among the given
		-- placeholders. If the placeholder exists, then this cursor
		-- will be pointing to it:
		cursor : pac_text_placeholders.cursor;

		
		procedure query_placeholder (
			p : in out type_text_placeholder)
		is begin
			case coordinates is
				when ABSOLUTE =>
					rotate_text_to (p, rotation);
					
				when RELATIVE =>
					rotate_text_by_2 (p, rotation);
			end case;
		end query_placeholder;
		
		
		
		procedure do_silkscreen is begin
			-- put_line ("do_silkscreen");
			
			case face is
				when TOP => 
					cursor := locate_placeholder (placeholders.silkscreen.top, meaning, index);

					-- If the specified placeholder exists, then do the actual rotation:
					if has_element (cursor) then
						-- put_line ("placeholder in silkscreen found");
						placeholders.silkscreen.top.update_element (cursor, query_placeholder'access);
					end if;

					
				when BOTTOM	=>
					cursor := locate_placeholder (placeholders.silkscreen.bottom, meaning, index);

					-- If the specified placeholder exists, then do the actual rotation:
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
		
					-- If the specified placeholder exists, then do the actual rotation:
					if has_element (cursor) then
						placeholders.assy_doc.top.update_element (cursor, query_placeholder'access);
					end if;

		
				when BOTTOM	=> 
					cursor := locate_placeholder (placeholders.assy_doc.bottom, meaning, index);

					-- If the specified placeholder exists, then do the actual rotation:
					if has_element (cursor) then
						placeholders.assy_doc.bottom.update_element (cursor, query_placeholder'access);
					end if;

			end case;		
		end do_assy_doc;
		
		
		
	begin
		-- put_line ("rotate_placeholder");
		
		case layer is
			when SILKSCREEN =>
				do_silkscreen;
					
			when ASSY_DOC =>
				do_assy_doc;
		end case;		
		
		-- put_line (to_string (placeholders));
	end rotate_placeholder;


	
	
	
	
	
	procedure propose_placeholders (
		placeholders		: in out type_text_placeholders;
		package_position	: in type_package_position;
		catch_zone			: in type_catch_zone;
		count				: in out natural;
		log_threshold		: in type_log_level)
	is
		use et_display.board;
		
		-- This cursor points to the placeholder being probed:		
		cursor : pac_text_placeholders.cursor;

		
		procedure query_placeholder (
			p : in out type_text_placeholder)
		is 
			-- Get the position of the placeholder.
			-- Currently we do not know whether pos is relative
			-- or absolute. The anchor mode of the placeholder
			-- provides the required information:
			pos : type_vector_model := get_place (p);
		begin
			log_indentation_up;
			
			case get_anchor_mode (p) is
				when ANCHOR_MODE_1 =>
					-- pos is the position relative to
					-- the package. It assumes that the package is
					-- not rotated (as defined in the package model).
					-- So the actual position of the placeholder candidate
					-- must be calculated using the package position. The 
					-- package position includes x/y, rotation and face.
				
					-- Rotate and move the position by the package position
					-- to get the absolute position of the placeholder:
					rotate_by (pos, get_rotation (package_position));
					move_by (pos, get_place (package_position));

				when ANCHOR_MODE_2 =>
					-- pos is the absolute position of the placeholder.
					-- Nothing elss to do.
					null;
			end case;
			
			-- log (text => "placeholder " & to_string (pos), level => log_threshold + 2);

			
			-- Test whether the placeholder is in the given catch zone:			
		 	if in_catch_zone (
				zone	=> catch_zone,
				point	=> pos)
			then
				log (text => "in catch zone", level => log_threshold + 2);
				
				-- Set the proposed flag:
				set_proposed (p);
				count := count + 1;				
			end if;
			
			log_indentation_down;
		end query_placeholder;
		
		
		
		procedure do_silkscreen_top is begin
			log (text => "do_silkscreen_top", level => log_threshold + 1);
			
			if silkscreen_enabled (top) then
				cursor := placeholders.silkscreen.top.first;
				while has_element (cursor) loop
					placeholders.silkscreen.top.update_element (
						cursor, query_placeholder'access);
						
					next (cursor);
				end loop;
			end if;
		end;
		 

		procedure do_silkscreen_bottom is begin
			log (text => "do_silkscreen_bottom", level => log_threshold + 1);
			
			if silkscreen_enabled (bottom) then
				cursor := placeholders.silkscreen.bottom.first;
				while has_element (cursor) loop
					placeholders.silkscreen.bottom.update_element (
						cursor, query_placeholder'access);
						
					next (cursor);
				end loop;		
			end if;
		end;


		
		procedure do_assy_doc_top is begin		
			log (text => "do_assy_doc_top", level => log_threshold + 1);		
			
			if assy_doc_enabled (top) then
				cursor := placeholders.assy_doc.top.first;
				while has_element (cursor) loop
					placeholders.assy_doc.top.update_element (
						cursor, query_placeholder'access);
						
					next (cursor);
				end loop;		
			end if;
		end;
		 

		procedure do_assy_doc_bottom is begin
			log (text => "do_assy_doc_bottom", level => log_threshold + 1);
			
			if assy_doc_enabled (bottom) then
				cursor := placeholders.assy_doc.bottom.first;
				while has_element (cursor) loop
					placeholders.assy_doc.bottom.update_element (
						cursor, query_placeholder'access);
						
					next (cursor);
				end loop;		
			end if;
		end;

		
	begin
		log (text => "propose_placeholders", level => log_threshold);
		log_indentation_up;
		
		do_silkscreen_top;
		do_silkscreen_bottom;
		
		do_assy_doc_top;
		do_assy_doc_bottom;
		
		log_indentation_down;
	end propose_placeholders;

	
	
	
	
	
	
	procedure modify_status (
		placeholders		: in out type_text_placeholders;
		layer				: in type_placeholder_layer;
		face				: in type_face;
		placeholder_cursor	: in pac_text_placeholders.cursor;
		operation			: in type_status_operation)
	is 
	
		procedure query_placeholder (
			p : in out type_text_placeholder)
		is 
		begin
			modify_status (p, operation);
		end;
	
	
	
		procedure do_silkscreen is begin
			case face is
				when TOP =>
					placeholders.silkscreen.top.update_element (
						placeholder_cursor, query_placeholder'access);

				when BOTTOM => 
					placeholders.silkscreen.bottom.update_element (
						placeholder_cursor, query_placeholder'access);
			end case;
		end;
		

		procedure do_assy_doc is begin
			case face is
				when TOP =>
					placeholders.assy_doc.top.update_element (
						placeholder_cursor, query_placeholder'access);

				when BOTTOM => 
					placeholders.assy_doc.bottom.update_element (
						placeholder_cursor, query_placeholder'access);
			end case;
		end;

		
		 
	begin
		case layer is
			when SILKSCREEN => do_silkscreen;
			when ASSY_DOC => do_assy_doc;
		end case;
	end modify_status;

	
	
	
	
	
	
	
	procedure reset_status (
		placeholders		: in out type_text_placeholders)
	is 
	
		procedure query_placeholder (
			p : in out type_text_placeholder)
		is begin
			reset_status (p);
		end;
		
	
		procedure do_silkscreen_top is
			c : pac_text_placeholders.cursor;
		begin
			c := placeholders.silkscreen.top.first;
			while has_element (c) loop
				placeholders.silkscreen.top.update_element (
					c, query_placeholder'access);
					
				next (c);
			end loop;
		end;

		
		procedure do_silkscreen_bottom is
			c : pac_text_placeholders.cursor;
		begin
			c := placeholders.silkscreen.bottom.first;
			while has_element (c) loop
				placeholders.silkscreen.bottom.update_element (
					c, query_placeholder'access);
					
				next (c);
			end loop;
		end;
		

		procedure do_assy_doc_top is
			c : pac_text_placeholders.cursor;
		begin
			c := placeholders.assy_doc.top.first;
			while has_element (c) loop
				placeholders.assy_doc.top.update_element (
					c, query_placeholder'access);
					
				next (c);
			end loop;
		end;

		
		procedure do_assy_doc_bottom is
			c : pac_text_placeholders.cursor;
		begin
			c := placeholders.assy_doc.bottom.first;
			while has_element (c) loop
				placeholders.assy_doc.bottom.update_element (
					c, query_placeholder'access);
					
				next (c);
			end loop;
		end;

		
	begin
		do_silkscreen_top;
		do_silkscreen_bottom;
		do_assy_doc_top;
		do_assy_doc_bottom;
	end reset_status;

	
	
	
	
	
	
	
	procedure get_first_placeholder (
		placeholders		: in type_text_placeholders;
		flag				: in type_flag;
		placeholder_cursor	: out pac_text_placeholders.cursor;
		layer				: out type_placeholder_layer;
		face				: out type_face;
		index				: out type_placeholder_index;
		log_threshold		: in type_log_level)
	is
		-- The search must be conducted for each layer,
		-- for each face and for each meaning.
		
		-- This flag is used in order to abort a search.
		proceed : aliased boolean := true;
		
		meaning : type_placeholder_meaning;
				
		
		procedure query_placeholder (c : in pac_text_placeholders.cursor) is
			p : type_text_placeholder renames element (c);
		begin
			-- Look only at placeholders that have the
			-- currently addressed meaning:
			if p.meaning = meaning then

				-- Test the status flag of the candidate placeholder.
				-- On match copy the candidate cursor to the result
				-- and abort the search because no more testing is required:
				case flag is
					when PROPOSED =>
						if is_proposed (p) then
							placeholder_cursor := c;
							proceed := false; -- abort 
						end if;

					when SELECTED =>
						if is_selected (p) then
							placeholder_cursor := c;
							proceed := false; -- abort
						end if;

					when others => null; -- CS
				end case;

				-- For each matching meaning, the index 
				-- must be incremented for the next placeholder:
				if proceed then
					index := index + 1;
				end if;
			end if;
		end query_placeholder;
		

		
		procedure do_silkscreen_top is begin
			log (text => "do_silkscreen_top", level => log_threshold + 1);
			log_indentation_up;
			
			layer := SILKSCREEN;
			face := TOP;
			
			-- Iterate for each available meaning:
			meaning := type_placeholder_meaning'first;
			while meaning /= type_placeholder_meaning'last loop
				index := 1; -- restart index
				iterate (placeholders.silkscreen.top, query_placeholder'access, proceed'access);
				
				-- Exit the loop once a placeholder has been found:
				if not proceed then exit; end if;
				
				-- Advance to next available meaning:
				meaning := type_placeholder_meaning'succ (meaning);
			end loop;
			
			log_indentation_down;
		end;

		
		procedure do_silkscreen_bottom is begin			
			log (text => "do_silkscreen_bottom", level => log_threshold + 1);
			log_indentation_up;
		
			if proceed then
				face := BOTTOM;
				
				-- Iterate for each available meaning:
				meaning := type_placeholder_meaning'first;
				while meaning /= type_placeholder_meaning'last loop
					index := 1; -- restart index
					iterate (placeholders.silkscreen.bottom, query_placeholder'access, proceed'access);
					
					-- Exit the loop once a placeholder has been found:
					if not proceed then exit; end if;
					
					-- Advance to next available meaning:
					meaning := type_placeholder_meaning'succ (meaning);
				end loop;
			end if;
			
			log_indentation_down;
		end do_silkscreen_bottom;

		
		procedure do_assy_doc_top is begin
			log (text => "do_assy_doc_top", level => log_threshold + 1);
			log_indentation_up;

			if proceed then
				layer := ASSY_DOC;
				face := TOP;
			
				-- Iterate for each available meaning:
				meaning := type_placeholder_meaning'first;
				while meaning /= type_placeholder_meaning'last loop
					index := 1; -- restart index
					iterate (placeholders.assy_doc.top, query_placeholder'access, proceed'access);
					
					-- Exit the loop once a placeholder has been found:
					if not proceed then exit; end if;
					
					-- Advance to next available meaning:
					meaning := type_placeholder_meaning'succ (meaning);
				end loop;
			end if;
			
			log_indentation_down;
		end;
		

		procedure do_assy_doc_bottom is begin			
			log (text => "do_assy_doc_bottom", level => log_threshold + 1);
			log_indentation_up;
		
			if proceed then
				face := BOTTOM;
				
				-- Iterate for each available meaning:
				meaning := type_placeholder_meaning'first;
				while meaning /= type_placeholder_meaning'last loop
					index := 1; -- restart index
					iterate (placeholders.assy_doc.bottom, query_placeholder'access, proceed'access);
					
					-- Exit the loop once a placeholder has been found:
					if not proceed then exit; end if;
					
					-- Advance to next available meaning:
					meaning := type_placeholder_meaning'succ (meaning);
				end loop;
			end if;
			
			log_indentation_down;
		end do_assy_doc_bottom;
		
		
	begin
		log (text => "get_first_placeholder", level => log_threshold);
		log_indentation_up;
		
		do_silkscreen_top;
		do_silkscreen_bottom;
		do_assy_doc_top;
		do_assy_doc_bottom;
		
		log_indentation_down;
	end get_first_placeholder;
	
	
	
	
	
	
	
	function get_placeholder_cursors (
		placeholders	: in type_text_placeholders;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return type_placeholder_cursors
	is
		result : type_placeholder_cursors;
		
		layer	: type_placeholder_layer;
		face	: type_face;
		index	: type_placeholder_index := 1;
		meaning	: type_placeholder_meaning;
		
		
		-- This procedure queries a given candidate placeholder,
		-- tests it s status flag and appends the cursor of
		-- the placeholder to the result:
		procedure query_placeholder (c : in pac_text_placeholders.cursor) is
			p : type_text_placeholder renames element (c);
			
			
			procedure collect is 
				pc : type_placeholder_cursor;
			begin
				pc.cursor := c;
				pc.index := index;
				index := index + 1; -- prepare for next placeholder
				
				case layer is
					when SILKSCREEN =>
						case face is
							when TOP	=> result.silkscreen.top.append (pc);
							when BOTTOM	=> result.silkscreen.bottom.append (pc);
						end case;
						
					when ASSY_DOC =>
						case face is
							when TOP	=> result.assy_doc.top.append (pc);
							when BOTTOM	=> result.assy_doc.bottom.append (pc);
						end case;
				end case;
			end collect;

			
		begin
			if p.meaning = meaning then
				case flag is
					when PROPOSED =>
						if is_proposed (p) then
							collect;
						end if;

					when SELECTED =>
						if is_selected (p) then
							collect;
						end if;

					when others => null; -- CS
				end case;
			end if;
		end query_placeholder;
		
		
	begin
		log (text => "get_placeholder_cursors", level => log_threshold);
		log_indentation_up;
		
		
		-- Iterate the placeholders in silkscreen top and bottom:
		layer := SILKSCREEN;
		face := TOP;
		index := 1;
		
		-- Iterate for each available meaning:
		meaning := type_placeholder_meaning'first;
		while meaning /= type_placeholder_meaning'last loop
			placeholders.silkscreen.top.iterate (query_placeholder'access);
			meaning := type_placeholder_meaning'succ (meaning);
		end loop;
		
		face := BOTTOM;
		index := 1;

		-- Iterate for each available meaning:
		meaning := type_placeholder_meaning'first;
		while meaning /= type_placeholder_meaning'last loop
			placeholders.silkscreen.bottom.iterate (query_placeholder'access);
			meaning := type_placeholder_meaning'succ (meaning);
		end loop;
		
		
		
		
		-- Iterate the placeholders in assy_doc top and bottom:
		layer := ASSY_DOC;
		face := TOP;
		index := 1;

		-- Iterate for each available meaning:
		meaning := type_placeholder_meaning'first;
		while meaning /= type_placeholder_meaning'last loop
			placeholders.assy_doc.top.iterate (query_placeholder'access);
			meaning := type_placeholder_meaning'succ (meaning);
		end loop;
		
		face := BOTTOM;
		index := 1;

		-- Iterate for each available meaning:
		meaning := type_placeholder_meaning'first;
		while meaning /= type_placeholder_meaning'last loop
			placeholders.assy_doc.bottom.iterate (query_placeholder'access);
			meaning := type_placeholder_meaning'succ (meaning);
		end loop;

		
		log_indentation_down;
		
		return result;
	end get_placeholder_cursors;
	

	
	
end et_device_placeholders.packages;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
