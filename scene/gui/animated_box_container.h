#ifndef ANIMATED_BOX_CONTAINER_H
#define ANIMATED_BOX_CONTAINER_H

#include "scene/animation/tween.h"
#include "scene/gui/container.h"

#define INTERP_PARAMETER_METHODS_DECLARE(name)                         \
	void _##name##_set_duration(real_t p_duration);                    \
	real_t _##name##_get_duration() const;                             \
                                                                       \
	void _##name##_set_trans_type(Tween::TransitionType p_trans_type); \
	Tween::TransitionType _##name##_get_trans_type() const;            \
                                                                       \
	void _##name##_set_ease_type(Tween::EaseType p_ease_type);         \
	Tween::EaseType _##name##_get_ease_type() const;                   \
                                                                       \
	void _##name##_set_delay(real_t p_delay);                          \
	real_t _##name##_get_delay() const

class AnimatedBoxContainer : public Container {
	GDCLASS(AnimatedBoxContainer, Container);

public:
	enum AlignMode {
		ALIGN_BEGIN,
		ALIGN_CENTER,
		ALIGN_END
	};

	enum AnimationType {
		ENTER,
		MOVE,
		LEAVE,

		COUNT
	};

private:
	struct InterpolationParameters {
		real_t duration;
		Tween::TransitionType trans_type;
		Tween::EaseType ease_type;
		real_t delay;
	};

	bool vertical;
	AlignMode align;

	Tween *movement_tween;
	Tween *transparency_tween;

	InterpolationParameters parameters[AnimationType::COUNT];

	Rect2 _get_fit_child_in_rect_rect(Control *p_child, const Rect2 &p_rect);
	void _resort();

protected:
	virtual void add_child_notify(Node *p_child);

	void _notification(int p_what);

	static void _bind_methods();

public:
	void free_child_animated(Control *child);

	void set_alignment(AlignMode p_align);
	AlignMode get_alignment() const;

	Tween *get_movement_tween() const;
	Tween *get_transparency_tween() const;

	void set_duration(AnimationType anim_type, real_t p_duration);
	real_t get_duration(AnimationType anim_type) const;

	void set_trans_type(AnimationType anim_type, Tween::TransitionType p_trans_type);
	Tween::TransitionType get_trans_type(AnimationType anim_type) const;

	void set_ease_type(AnimationType anim_type, Tween::EaseType p_ease_type);
	Tween::EaseType get_ease_type(AnimationType anim_type) const;

	void set_delay(AnimationType anim_type, real_t p_delay);
	real_t get_delay(AnimationType anim_type) const;

	INTERP_PARAMETER_METHODS_DECLARE(enter);
	INTERP_PARAMETER_METHODS_DECLARE(move);
	INTERP_PARAMETER_METHODS_DECLARE(leave);

	virtual Size2 get_minimum_size() const;

	AnimatedBoxContainer(bool p_vertical = false);
};

class AnimatedHBoxContainer : public AnimatedBoxContainer {

	GDCLASS(AnimatedHBoxContainer, AnimatedBoxContainer);

public:
	AnimatedHBoxContainer() :
			AnimatedBoxContainer(false) {}
};

class AnimatedVBoxContainer : public AnimatedBoxContainer {

	GDCLASS(AnimatedVBoxContainer, AnimatedBoxContainer);

public:
	AnimatedVBoxContainer() :
			AnimatedBoxContainer(true) {}
};

VARIANT_ENUM_CAST(AnimatedBoxContainer::AlignMode);
VARIANT_ENUM_CAST(AnimatedBoxContainer::AnimationType);

#endif // ANIMATED_BOX_CONTAINER_H