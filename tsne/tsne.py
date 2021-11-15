# That's an impressive list of imports.
import numpy as np
from numpy import linalg

import sklearn
from sklearn.manifold import TSNE
from sklearn.datasets import load_digits

RS = 20150101

# We'll use matplotlib for graphics.
import matplotlib.pyplot as plt
import matplotlib.patheffects as PathEffects

# We import seaborn to make nice plots.
import seaborn as sns
sns.set_style('darkgrid')
sns.set_palette('muted')
sns.set_context("notebook", font_scale=1.5,
                rc={"lines.linewidth": 2.5})

# We'll generate an animation with matplotlib and moviepy.
from moviepy.video.io.bindings import mplfig_to_npimage
import moviepy.editor as mpy

import pandas as pd

def load_data(excel_file,group_name='Clus_km'):
    # df=pd.read_excel(excel_file)
    # df.pop('author_id')
    # print(df.head(3))
    # grouped=df.groupby(by=group_name,axis=0)
    # data = np.vstack([grouped.get_group(i) for i in range_section])
    # X,y=data[:,:-1],data[:,-1]
    digits = load_digits()
    X = np.vstack([digits.data[digits.target == i]
                   for i in range_section])
    y = np.hstack([digits.target[digits.target == i]
                   for i in range_section])

    return X,y

# 修改颜色
def modify_colors(colors):
    # 簇/分类和颜色对应关系
    cluster_color_map={1:'#B04E3E',2:'#A19371',3:'#1E4288',4:'#F2C351',5:'#8C8E8D',6:'#48933A'}
    color_box=[]
    for i in colors:
        color=cluster_color_map.get(int(i),1)
        color_box.append(color)
    return color_box

def scatter(x, colors):
    # We create a scatter plot.
    pic_colors=modify_colors(colors)
    f = plt.figure(figsize=(8, 8))
    ax = plt.subplot(aspect='equal')
    sc = ax.scatter(x[:,0], x[:,1], lw=0, s=40,
                    c=pic_colors)
    plt.xlim(-25, 25)
    plt.ylim(-25, 25)
    ax.axis('off')
    ax.axis('tight')
    # We add the labels for each digit.
    txts = []
    for i in range_section:
        # Position of each label.
        xtext, ytext = np.median(x[colors == i, :], axis=0)
        txt = ax.text(xtext, ytext, str(i), fontsize=24)
        txt.set_path_effects([
            PathEffects.Stroke(linewidth=5, foreground="w"),
            PathEffects.Normal()])
        txts.append(txt)
    return f, ax, sc, txts

positions = []

def _gradient_descent(objective, p0, it, n_iter,n_iter_check=1, n_iter_without_progress=30,
                      momentum=0.5, learning_rate=1000.0, min_gain=0.01,
                      min_grad_norm=1e-7, min_error_diff=1e-7, verbose=0,
                      args=[],kwargs={}):
    
    # The documentation of this function can be found in scikit-learn's code.
    p = p0.copy().ravel()
    update = np.zeros_like(p)
    gains = np.ones_like(p)
    error = np.finfo(np.float).max
    best_error = np.finfo(np.float).max
    best_iter = 0
    for i in range(it, n_iter):
        # We save the current position.
        positions.append(p.copy())
        new_error, grad = objective(p, *args)
        error_diff = np.abs(new_error - error)
        error = new_error
        grad_norm = linalg.norm(grad)

        if error < best_error:
            best_error = error
            best_iter = i
        elif i - best_iter > n_iter_without_progress:
            break
        if min_grad_norm >= grad_norm:
            break
        if min_error_diff >= error_diff:
            break

        inc = update * grad >= 0.0
        dec = np.invert(inc)
        gains[inc] += 0.05
        gains[dec] *= 0.95
        np.clip(gains, min_gain, np.inf)
        grad *= gains
        update = momentum * update - learning_rate * grad
        p += update
    return p, error, i

def make_frame_mpl(t):
    i = int(t*40)
    x = X_iter[..., i]
    sc.set_offsets(x)
    for j, txt in zip(range_section, txts):
        xtext, ytext = np.median(x[y == j, :], axis=0)
        txt.set_x(xtext)
        txt.set_y(ytext)
    return mplfig_to_npimage(f)


range_section=range(1,7)

if __name__ == '__main__':
    excel_file='mm6.xlsx'
    X,y=load_data(excel_file)
    print(X.shape)
    print(y.shape)
    sklearn.manifold._t_sne._gradient_descent = _gradient_descent
    tsne= TSNE(random_state=RS)
    X_proj=tsne.fit_transform(X)

    X_iter = np.dstack(position.reshape(-1, 2)
                       for position in positions)
    f, ax, sc, txts = scatter(X_iter[..., -1], y)
    animation = mpy.VideoClip(make_frame_mpl,
                              duration=X_iter.shape[2]/40.)
    video_title="tsne.mp4"
    gif_title="tsne.gif"
    # fps 是每秒播放多少帧
    animation.write_videofile(video_title, codec='mpeg4',fps=10)
    animation.write_gif(gif_title,fps=10)
