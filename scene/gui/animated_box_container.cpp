#include "animated_box_container.h"

#include "core/engine.h"

#define INTERP_PARAMETER_METHODS_IMPLEMENT(name, anim_type)                                   \
	void AnimatedBoxContainer::_##name##_set_duration(real_t p_duration) {                    \
		parameters[anim_type].duration = p_duration;                                          \
		_resort();                                                                            \
	}                                                                                         \
                                                                                              \
	real_t AnimatedBoxContainer::_##name##_get_duration() const {                             \
		return parameters[anim_type].duration;                                                \
	}                                                                                         \
                                                                                              \
	void AnimatedBoxContainer::_##name##_set_trans_type(Tween::TransitionType p_trans_type) { \
		parameters[anim_type].trans_type = p_trans_type;                                      \
		_resort();                                                                            \
	}                                                                                         \
                                                                                              \
	Tween::TransitionType AnimatedBoxContainer::_##name##_get_trans_type() const {            \
		return parameters[anim_type].trans_type;                                              \
	}                                                                                         \
                                                                                              \
	void AnimatedBoxContainer::_##name##_set_ease_type(Tween::EaseType p_ease_type) {         \
		parameters[anim_type].ease_type = p_ease_type;                                        \
		_resort();                                                                            \
	}                                                                                         \
                                                                                              \
	Tween::EaseType AnimatedBoxContainer::_##name##_get_ease_type() const {                   \
		return parameters[anim_type].ease_type;                                               \
	}                                                                                         \
                                                                                              \
	void AnimatedBoxContainer::_##name##_set_delay(real_t p_delay) {                          \
		parameters[anim_type].delay = p_delay;                                                \
		_resort();                                                                            \
	}                                                                                         \
                                                                                              \
	real_t AnimatedBoxContainer::_##name##_get_delay() const {                                \
		return parameters[anim_type].delay;                                                   \
	}

#define INTERP_PARAMETER_METHODS_BIND(name)                                                                                     \
	ClassDB::bind_method(D_METHOD("_" #name "_get_duration"), &AnimatedBoxContainer::_##name##_get_duration);                   \
	ClassDB::bind_method(D_METHOD("_" #name "_set_duration", "duration"), &AnimatedBoxContainer::_##name##_set_duration);       \
	ClassDB::bind_method(D_METHOD("_" #name "_get_trans_type"), &AnimatedBoxContainer::_##name##_get_trans_type);               \
	ClassDB::bind_method(D_METHOD("_" #name "_set_trans_type", "trans_type"), &AnimatedBoxContainer::_##name##_set_trans_type); \
	ClassDB::bind_method(D_METHOD("_" #name "_get_ease_type"), &AnimatedBoxContainer::_##name##_get_ease_type);                 \
	ClassDB::bind_method(D_METHOD("_" #name "_set_ease_type", "ease_type"), &AnimatedBoxContainer::_##name##_set_ease_type);    \
	ClassDB::bind_method(D_METHOD("_" #name "_get_delay"), &AnimatedBoxContainer::_##name##_get_delay);                         \
	ClassDB::bind_method(D_METHOD("_" #name "_set_delay", "delay"), &AnimatedBoxContainer::_##name##_set_delay)

#define INTERT_PARAMETER_PROPERTIES_BIND(name)                                                                                                                                                                       \
	ADD_PROPERTY(PropertyInfo(Variant::REAL, #name "_duration", PROPERTY_HINT_RANGE, "0,1,0.001"), "_" #name "_set_duration", "_" #name "_get_duration");                                                            \
	ADD_PROPERTY(PropertyInfo(Variant::INT, #name "_trans_type", PROPERTY_HINT_ENUM, "Linear,Sine,Quint,Quart,Quad,Expo,Elastic,Cubic,Circ,Bounce,Back"), "_" #name "_set_trans_type", "_" #name "_get_trans_type"); \
	ADD_PROPERTY(PropertyInfo(Variant::INT, #name "_ease_type", PROPERTY_HINT_ENUM, "Ease In,Ease Out,Ease In Out,Ease Out In"), "_" #name "_set_ease_type", "_" #name "_get_ease_type");                            \
	ADD_PROPERTY(PropertyInfo(Variant::REAL, #name "_delay", PROPERTY_HINT_RANGE, "0,1,0.001"), "_" #name "_set_delay", "_" #name "_get_delay")

struct _MinSizeCache {
	int min_size;
	bool will_stretch;
	int final_size;
};

void AnimatedBoxContainer::_resort() {
	/** First pass, determine minimum size AND amount of stretchable elements */

	Size2i new_size = get_size();

	int sep = get_constant("separation"); //,vertical?"VBoxContainer":"HBoxContainer");

	bool first = true;
	int children_count = 0;
	int stretch_min = 0;
	int stretch_avail = 0;
	float stretch_ratio_total = 0;
	Map<Control *, _MinSizeCache> min_size_cache;

	for (int i = 0; i < get_child_count(); i++) {
		Control *c = Object::cast_to<Control>(get_child(i));
		if (!c || !c->is_visible_in_tree())
			continue;
		if (c->is_set_as_toplevel())
			continue;

		Size2i size = c->get_combined_minimum_size();
		_MinSizeCache msc;

		if (vertical) { /* VERTICAL */
			stretch_min += size.height;
			msc.min_size = size.height;
			msc.will_stretch = c->get_v_size_flags() & SIZE_EXPAND;

		} else { /* HORIZONTAL */
			stretch_min += size.width;
			msc.min_size = size.width;
			msc.will_stretch = c->get_h_size_flags() & SIZE_EXPAND;
		}

		if (msc.will_stretch) {
			stretch_avail += msc.min_size;
			stretch_ratio_total += c->get_stretch_ratio();
		}
		msc.final_size = msc.min_size;
		min_size_cache[c] = msc;
		children_count++;
	}

	if (children_count == 0)
		return;

	int stretch_max = (vertical ? new_size.height : new_size.width) - (children_count - 1) * sep;
	int stretch_diff = stretch_max - stretch_min;
	if (stretch_diff < 0) {
		//avoid negative stretch space
		stretch_diff = 0;
	}

	stretch_avail += stretch_diff; //available stretch space.
	/** Second, pass successively to discard elements that can't be stretched, this will run while stretchable
		elements exist */

	bool has_stretched = false;
	while (stretch_ratio_total > 0) { // first of all, don't even be here if no stretchable objects exist

		has_stretched = true;
		bool refit_successful = true; //assume refit-test will go well
		float error = 0; // Keep track of accumulated error in pixels

		for (int i = 0; i < get_child_count(); i++) {

			Control *c = Object::cast_to<Control>(get_child(i));
			if (!c || !c->is_visible_in_tree())
				continue;
			if (c->is_set_as_toplevel())
				continue;

			ERR_FAIL_COND(!min_size_cache.has(c));
			_MinSizeCache &msc = min_size_cache[c];

			if (msc.will_stretch) { //wants to stretch
				//let's see if it can really stretch
				float final_pixel_size = stretch_avail * c->get_stretch_ratio() / stretch_ratio_total;
				// Add leftover fractional pixels to error accumulator
				error += final_pixel_size - (int)final_pixel_size;
				if (final_pixel_size < msc.min_size) {
					//if available stretching area is too small for widget,
					//then remove it from stretching area
					msc.will_stretch = false;
					stretch_ratio_total -= c->get_stretch_ratio();
					refit_successful = false;
					stretch_avail -= msc.min_size;
					msc.final_size = msc.min_size;
					break;
				} else {
					msc.final_size = final_pixel_size;
					// Dump accumulated error if one pixel or more
					if (error >= 1) {
						msc.final_size += 1;
						error -= 1;
					}
				}
			}
		}

		if (refit_successful) //uf refit went well, break
			break;
	}

	/** Final pass, draw and stretch elements **/

	int ofs = 0;
	if (!has_stretched) {
		switch (align) {
			case ALIGN_BEGIN:
				break;
			case ALIGN_CENTER:
				ofs = stretch_diff / 2;
				break;
			case ALIGN_END:
				ofs = stretch_diff;
				break;
		}
	}

	first = true;
	int idx = 0;

	movement_tween->remove_all();

	for (int i = 0; i < get_child_count(); i++) {

		Control *c = Object::cast_to<Control>(get_child(i));
		if (!c || !c->is_visible_in_tree())
			continue;
		if (c->is_set_as_toplevel())
			continue;

		_MinSizeCache &msc = min_size_cache[c];

		if (first)
			first = false;
		else
			ofs += sep;

		int from = ofs;
		int to = ofs + msc.final_size;

		if (msc.will_stretch && idx == children_count - 1) {
			//adjust so the last one always fits perfect
			//compensating for numerical imprecision

			to = vertical ? new_size.height : new_size.width;
		}

		int size = to - from;

		Rect2 rect;

		if (vertical) {

			rect = Rect2(0, from, new_size.width, size);
		} else {

			rect = Rect2(from, 0, size, new_size.height);
		}

		rect = _get_fit_child_in_rect_rect(c, rect);

		c->set_rotation(0);
		c->set_scale(Vector2(1, 1));

		InterpolationParameters move = parameters[AnimationType::MOVE];
		InterpolationParameters enter = parameters[AnimationType::ENTER];

		if (c->has_meta("_is_new")) {
			// the control is new, set position and animate transparency.
			c->remove_meta("_is_new");
			c->set_position(rect.get_position());
			c->set_size(rect.get_size());

			if (!Engine::get_singleton()->is_editor_hint()) {
				// do not animate transparency in the editor.
				Color modulate = c->get_modulate();
				float c_alpha = modulate.a;
				modulate.a = 0.0;
				c->set_modulate(modulate);

				bool wait_movement = get_child_count() > 1 && (align == ALIGN_CENTER || (align == ALIGN_BEGIN && i < get_child_count() - 1) || (align == ALIGN_END && i > 0));
				real_t wait_movement_delay = (wait_movement ? move.delay + move.duration : 0.0);

				transparency_tween->interpolate_property(c, NodePath("modulate:a"), 0.0, c_alpha, enter.duration, enter.trans_type, enter.ease_type, enter.delay + wait_movement_delay);
			}
		} else {
			movement_tween->interpolate_property(c, NodePath("rect_position"), c->get_position(), rect.get_position(), move.duration, move.trans_type, move.ease_type, move.delay);
			movement_tween->interpolate_property(c, NodePath("rect_size"), c->get_size(), rect.get_size(), move.duration, move.trans_type, move.ease_type, move.delay);
		}

		ofs = to;
		idx++;
	}

	movement_tween->start();
	transparency_tween->start();
}

Rect2 AnimatedBoxContainer::_get_fit_child_in_rect_rect(Control *p_child, const Rect2 &p_rect) {
	ERR_FAIL_COND_V(!p_child, Rect2());
	ERR_FAIL_COND_V(p_child->get_parent() != this, Rect2());

	Size2 minsize = p_child->get_combined_minimum_size();
	Rect2 r = p_rect;

	if (!(p_child->get_h_size_flags() & SIZE_FILL)) {
		r.size.x = minsize.width;
		if (p_child->get_h_size_flags() & SIZE_SHRINK_END) {
			r.position.x += p_rect.size.width - minsize.width;
		} else if (p_child->get_h_size_flags() & SIZE_SHRINK_CENTER) {
			r.position.x += Math::floor((p_rect.size.x - minsize.width) / 2);
		} else {
			r.position.x += 0;
		}
	}

	if (!(p_child->get_v_size_flags() & SIZE_FILL)) {
		r.size.y = minsize.y;
		if (p_child->get_v_size_flags() & SIZE_SHRINK_END) {
			r.position.y += p_rect.size.height - minsize.height;
		} else if (p_child->get_v_size_flags() & SIZE_SHRINK_CENTER) {
			r.position.y += Math::floor((p_rect.size.y - minsize.height) / 2);
		} else {
			r.position.y += 0;
		}
	}

	return r;
}

void AnimatedBoxContainer::add_child_notify(Node *p_child) {
	Container::add_child_notify(p_child);

	Control *control = Object::cast_to<Control>(p_child);
	if (!control)
		return;

	control->set_meta("_is_new", true);
}

Size2 AnimatedBoxContainer::get_minimum_size() const {
	/* Calculate MINIMUM SIZE */

	Size2i minimum;
	int sep = get_constant("separation"); //,vertical?"VBoxContainer":"HBoxContainer");

	bool first = true;

	for (int i = 0; i < get_child_count(); i++) {
		Control *c = Object::cast_to<Control>(get_child(i));
		if (!c)
			continue;
		if (c->is_set_as_toplevel())
			continue;

		if (!c->is_visible()) {
			continue;
		}

		Size2i size = c->get_combined_minimum_size();

		if (vertical) { /* VERTICAL */

			if (size.width > minimum.width) {
				minimum.width = size.width;
			}

			minimum.height += size.height + (first ? 0 : sep);

		} else { /* HORIZONTAL */

			if (size.height > minimum.height) {
				minimum.height = size.height;
			}

			minimum.width += size.width + (first ? 0 : sep);
		}

		first = false;
	}

	return minimum;
}

void AnimatedBoxContainer::_notification(int p_what) {
	switch (p_what) {
		case NOTIFICATION_SORT_CHILDREN: {
			_resort();
		} break;
		case NOTIFICATION_THEME_CHANGED: {
			minimum_size_changed();
		} break;
	}
}

void AnimatedBoxContainer::free_child_animated(Control *child) {
	ERR_FAIL_COND(!child);
	ERR_FAIL_COND(child->get_parent() != this);

	InterpolationParameters leave = parameters[AnimationType::LEAVE];

	if (!Engine::get_singleton()->is_editor_hint() &&
			transparency_tween->interpolate_property(child, NodePath("modulate:a"), child->get_modulate().a, 0.0, leave.duration, leave.trans_type, leave.ease_type, leave.delay)) {
		transparency_tween->start();

		int err = get_tree()->create_timer(leave.duration + leave.delay)->connect("timeout", child, "queue_free");
		if (err)
			ERR_PRINT("Error connecting to the queue_free.");
	} else {
		child->queue_delete();
	}
}

void AnimatedBoxContainer::set_alignment(AlignMode p_align) {
	align = p_align;
	_resort();
}

AnimatedBoxContainer::AlignMode AnimatedBoxContainer::get_alignment() const {
	return align;
}

Tween *AnimatedBoxContainer::get_movement_tween() const {
	return movement_tween;
}

Tween *AnimatedBoxContainer::get_transparency_tween() const {
	return transparency_tween;
}

void AnimatedBoxContainer::set_duration(AnimationType anim_type, real_t p_duration) {
	ERR_FAIL_INDEX(anim_type, AnimationType::COUNT);

	parameters[anim_type].duration = p_duration;
	_resort();
}

real_t AnimatedBoxContainer::get_duration(AnimationType anim_type) const {
	ERR_FAIL_INDEX_V(anim_type, AnimationType::COUNT, 0.0);

	return parameters[anim_type].duration;
}

void AnimatedBoxContainer::set_trans_type(AnimationType anim_type, Tween::TransitionType p_trans_type) {
	ERR_FAIL_INDEX(anim_type, AnimationType::COUNT);

	parameters[anim_type].trans_type = p_trans_type;
	_resort();
}

Tween::TransitionType AnimatedBoxContainer::get_trans_type(AnimationType anim_type) const {
	ERR_FAIL_INDEX_V(anim_type, AnimationType::COUNT, Tween::TransitionType::TRANS_LINEAR);

	return parameters[anim_type].trans_type;
}

void AnimatedBoxContainer::set_ease_type(AnimationType anim_type, Tween::EaseType p_ease_type) {
	ERR_FAIL_INDEX(anim_type, AnimationType::COUNT);

	parameters[anim_type].ease_type = p_ease_type;
	_resort();
}

Tween::EaseType AnimatedBoxContainer::get_ease_type(AnimationType anim_type) const {
	ERR_FAIL_INDEX_V(anim_type, AnimationType::COUNT, Tween::EaseType::EASE_IN_OUT);

	return parameters[anim_type].ease_type;
}

void AnimatedBoxContainer::set_delay(AnimationType anim_type, real_t p_delay) {
	ERR_FAIL_INDEX(anim_type, AnimationType::COUNT);

	parameters[anim_type].delay = p_delay;
	_resort();
}

real_t AnimatedBoxContainer::get_delay(AnimationType anim_type) const {
	ERR_FAIL_INDEX_V(anim_type, AnimationType::COUNT, 0.0);

	return parameters[anim_type].delay;
}

INTERP_PARAMETER_METHODS_IMPLEMENT(enter, AnimationType::ENTER);
INTERP_PARAMETER_METHODS_IMPLEMENT(move, AnimationType::MOVE);
INTERP_PARAMETER_METHODS_IMPLEMENT(leave, AnimationType::LEAVE);

AnimatedBoxContainer::AnimatedBoxContainer(bool p_vertical) {
	movement_tween = memnew(Tween);
	movement_tween->set_name("_movement_tween");
	add_child(movement_tween);

	transparency_tween = memnew(Tween);
	transparency_tween->set_name("_transparency_tween");
	add_child(transparency_tween);

	for (int i = 0; i < AnimationType::COUNT; i++) {
		parameters[i].duration = 0.1;
		parameters[i].trans_type = Tween::TransitionType::TRANS_LINEAR;
		parameters[i].ease_type = Tween::EaseType::EASE_IN_OUT;
		parameters[i].delay = 0.0;
	}

	vertical = p_vertical;
	align = ALIGN_BEGIN;
	//set_ignore_mouse(true);
	set_mouse_filter(MOUSE_FILTER_PASS);
}

void AnimatedBoxContainer::_bind_methods() {
	ClassDB::bind_method(D_METHOD("free_child_animated", "child"), &AnimatedBoxContainer::free_child_animated);
	ClassDB::bind_method(D_METHOD("get_alignment"), &AnimatedBoxContainer::get_alignment);
	ClassDB::bind_method(D_METHOD("set_alignment", "alignment"), &AnimatedBoxContainer::set_alignment);
	ClassDB::bind_method(D_METHOD("get_movement_tween"), &AnimatedBoxContainer::get_movement_tween);
	ClassDB::bind_method(D_METHOD("get_transparency_tween"), &AnimatedBoxContainer::get_transparency_tween);
	ClassDB::bind_method(D_METHOD("get_duration", "anim_type"), &AnimatedBoxContainer::get_duration);
	ClassDB::bind_method(D_METHOD("set_duration", "anim_type", "duration"), &AnimatedBoxContainer::set_duration);
	ClassDB::bind_method(D_METHOD("get_trans_type", "anim_type"), &AnimatedBoxContainer::get_trans_type);
	ClassDB::bind_method(D_METHOD("set_trans_type", "anim_type", "trans_type"), &AnimatedBoxContainer::set_trans_type);
	ClassDB::bind_method(D_METHOD("get_ease_type", "anim_type"), &AnimatedBoxContainer::get_ease_type);
	ClassDB::bind_method(D_METHOD("set_ease_type", "anim_type", "ease_type"), &AnimatedBoxContainer::set_ease_type);
	ClassDB::bind_method(D_METHOD("get_delay", "anim_type"), &AnimatedBoxContainer::get_delay);
	ClassDB::bind_method(D_METHOD("set_delay", "anim_type", "delay"), &AnimatedBoxContainer::set_delay);

	INTERP_PARAMETER_METHODS_BIND(enter);
	INTERP_PARAMETER_METHODS_BIND(move);
	INTERP_PARAMETER_METHODS_BIND(leave);

	BIND_ENUM_CONSTANT(ALIGN_BEGIN);
	BIND_ENUM_CONSTANT(ALIGN_CENTER);
	BIND_ENUM_CONSTANT(ALIGN_END);

	BIND_ENUM_CONSTANT(ENTER);
	BIND_ENUM_CONSTANT(MOVE);
	BIND_ENUM_CONSTANT(LEAVE);

	ADD_PROPERTY(PropertyInfo(Variant::INT, "alignment", PROPERTY_HINT_ENUM, "Begin,Center,End"), "set_alignment", "get_alignment");

	ADD_GROUP("Enter", "enter_");
	INTERT_PARAMETER_PROPERTIES_BIND(enter);

	ADD_GROUP("Move", "move_");
	INTERT_PARAMETER_PROPERTIES_BIND(move);

	ADD_GROUP("Leave", "leave_");
	INTERT_PARAMETER_PROPERTIES_BIND(leave);
}
